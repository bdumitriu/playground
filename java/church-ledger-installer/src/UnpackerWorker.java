import java.util.Enumeration;
import java.util.zip.ZipFile;
import java.util.zip.ZipException;
import java.util.zip.ZipEntry;
import java.io.*;

/**
 * Worker class which extracts a zip file.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 */
public class UnpackerWorker extends Worker
{
	/**
	 * @param fileName the name of the zip file to unpack
	 * @param extractRoot the root directory where to extract the contents of the archive
	 */
	public UnpackerWorker(String fileName, String extractRoot)
	{
		super();
		this.fileName = fileName;
		if (extractRoot.endsWith(System.getProperty("file.separator")))
		{
			this.extractRoot = extractRoot;
		}
		else
		{
			this.extractRoot = extractRoot + System.getProperty("file.separator");
		}
		totalSize = -1;
	}

	public void perform() throws Exception
	{
		try
		{
			zipFile = new ZipFile(fileName);
		}
		catch (ZipException e)
		{
			throw new Exception("There was a problem with the zip file: " + fileName + ".");
		}
		catch (SecurityException e)
		{
			throw new Exception("The zip file (" + fileName + ") cannot be accessed (no read access?).");
		}
		catch (IOException e)
		{
			throw new Exception("The zip file (" + fileName + ") could not be opened.");
		}

		try
		{
			// make sure totalSize data member is computed
			getTotalSize();

			runningSize = 0;
			runningPercentile = -100;

			updateRunningSize(0);

			Enumeration entries = zipFile.entries();
			for (; entries.hasMoreElements();)
			{
				ZipEntry ze = (ZipEntry) entries.nextElement();
				String fName = extractRoot + ze.getName();

				if (ze.isDirectory())
				{
					try
					{
						new File(fName).mkdirs();
					}
					catch (SecurityException e)
					{
						throw new Exception("Error creating directory " + fName +
							" while unpacking zip archive due to missing access rights.");
					}
				}
				else
				{
					OutputStream outStream = null;
					try
					{
						outStream = new FileOutputStream(fName);
					}
					catch (FileNotFoundException e)
					{
						throw new Exception("Error creating file " + fName +
							" while unpacking zip archive (some name conflict?).");
					}
					catch (SecurityException e)
					{
						throw new Exception("Error creating file " + fName +
							" while unpacking zip archive due to missing access rights.");
					}

					InputStream inStream = null;
					try
					{
						inStream = zipFile.getInputStream(ze);
					}
					catch (java.util.zip.ZipException e)
					{
						throw new Exception("Error unpacking zip archive: " +
							fileName + ".");
					}
					catch (IOException e)
					{
						throw new Exception("Error unpacking zip archive: " +
							fileName + ".");
					}

					int nrBytes;
					byte buf[] = new byte[80000];
					while ((nrBytes = inStream.read(buf)) != -1)
					{
						outStream.write(buf, 0, nrBytes);
						updateRunningSize(nrBytes);
					}

					outStream.close();
					inStream.close();
				}
			}
		}
		catch (IOException e)
		{
			throw new Exception("Error unpacking zip archive: " + fileName + ".");
		}
	}

	private void updateRunningSize(long increment)
	{
		runningSize += increment;
		long tmpPercentile = getPercentile(runningSize);
		if (tmpPercentile - runningPercentile >= percentile)
		{
			runningPercentile = tmpPercentile;
			setChanged();
			notifyObservers(new ObserverEvent(getWorkerNr(), (int) runningPercentile));
		}
	}
	/**
	 * Returns the total size of the unpacked archive.
	 */
	public long getTotalSize()
	{
		if (totalSize != -1)
		{
			return totalSize;
		}
		else
		{
			Enumeration entries = zipFile.entries();
			totalSize = 0;
			for (; entries.hasMoreElements();)
			{
				ZipEntry ze = (ZipEntry) entries.nextElement();
				totalSize += ze.getSize();
			}

			return totalSize;
		}
	}

	/**
	 * Returns a number between 1 and 100 which represents the percentile of totalSize which <code>size</code>
	 * represents. For efficiency reasons, no consistency checks are made, so use with care.
	 */
	private long getPercentile(long size)
	{
		long temp = 100 * size;
		return temp / totalSize;
	}

	private String fileName;
	private ZipFile zipFile;
	private String extractRoot;
	private long totalSize;
	private long runningSize;
	private long runningPercentile;
}
