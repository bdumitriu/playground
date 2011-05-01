
import java.util.*;
import java.io.*;

/**
 * Monitors a directory for changes.
 * 
 * @author  Bogdan Dumitriu
 * @date    23.06.2005
 * @version 0.1
 */
public class DirectoryMonitor extends Observable implements Runnable, Observer
{
	public static void main(String[] args)
	{
		DirectoryMonitor dm = new DirectoryMonitor("", "");
		Thread dmt = new Thread(dm);
		dmt.start();
		//dm.terminate();
	}

	public DirectoryMonitor(String dir, String copyToDir)
	{
		this.dir = dir;
		this.copyToDir = new File(copyToDir);
		this.copyToDir.mkdirs();
		stop = false;
		files = new HashMap<String,MyMapEntry>();
		addObserver(this);
		separator = "\\";// System.getProperty("file.separator");
		fileNr = 1;
	}

	public void terminate()
	{
		stop = true;
	}

	public void run()
	{
		while (!stop)
		{
			try
			{
				ArrayList<String> newFiles = updateFileList();
				if (newFiles.size() != 0)
				{
					setChanged();
					notifyObservers(newFiles);
				}

				Thread.sleep(1000);
			}
			catch (InterruptedException e)
			{}
		}
	}

	private ArrayList<String> updateFileList()
	{
		//System.out.println("Running updateFileList()...");
		File f = new File(dir);
		File[] allFiles = f.listFiles();
		ArrayList<String> result = new ArrayList<String>();

		if (allFiles != null)
		{
			//System.out.println("allFiles was not null");
			for (File file : allFiles)
			{
				//System.out.println("Processing file " + file.getAbsolutePath());
				String fileName = file.getAbsolutePath();
				Long fileSize = new Long(file.length());
				MyMapEntry mme = files.get(fileName);
				if (mme == null)
				{
					files.put(fileName, new MyMapEntry(fileSize, false));
				}
				else
				{
					Long oldFileSize = mme.getSize();
					if (oldFileSize.equals(fileSize) && !(mme.isNotified()))
					{
						result.add(fileName);
						files.put(fileName, new MyMapEntry(fileSize, true));
					}
					else if (!(mme.isNotified()))
					{
						files.put(fileName, new MyMapEntry(fileSize, false));
					}
				}
			}
		}

		//System.out.println("Returning result with " + result.size() + " entries.");
		return result;
	}

	public void update(Observable o, Object arg)
	{
		if (arg instanceof ArrayList)
		{
			ArrayList<String> newFiles = (ArrayList<String>) arg;
			for (String fileName : newFiles)
			{
				//System.out.println("new file: " + fileName);
				File fFrom = new File(fileName);
				File fTo = new File(copyToDir.getAbsolutePath() + separator + "" +
					format3(fileNr) + "");
				copyFile(fFrom, fTo);
				fileNr++;
			}
		}
	}

	private String format3(int n)
	{
		if (0 <= n && n < 10)
		{
			return "00" + new Integer(n).toString();
		}
		else if (10 <= n && n < 100)
		{
			return "0" + new Integer(n).toString();
		}
		else
		{
			return new Integer(n).toString();
		}
	}

	private void copyFile(File fFrom, File fTo)
	{
		try
		{
			FileOutputStream outStream = new FileOutputStream(fTo);
			FileInputStream inStream = new FileInputStream(fFrom);
			int nrBytes;
			byte buf[] = new byte[80000];
			while ((nrBytes = inStream.read(buf)) != -1)
			{
				outStream.write(buf, 0, nrBytes);
			}
			outStream.close();
			inStream.close();
		}
		catch (Exception e)
		{}
	}

	private HashMap<String,MyMapEntry> files;
	private String dir;
	private File copyToDir;
	private boolean stop;
	private String separator;
	private int fileNr;
}

class MyMapEntry
{
	public MyMapEntry(Long size, boolean notified)
	{
		this.size = size;
		this.notified = notified;
	}

	public Long getSize()
	{
		return size;
	}

	public void setSize(Long size)
	{
		this.size = size;
	}

	public boolean isNotified()
	{
		return notified;
	}

	public void setNotified(boolean notified)
	{
		this.notified = notified;
	}

	private Long size;
	private boolean notified;
}
