
import java.util.*;
import java.io.File;

/**
 * Worker class which installs a .msi package.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 */
public class MSIWorker extends Worker
{
	/**
	 *
	 * @param fileName the .msi file
	 * @param extractRoot the path where you want to install the software
	 * @param options all other options for the .msi file, except INSTALLDIR, which is generated automatically based
	 *	on the extractRoot (can be null)
	 */
	public MSIWorker(String fileName, String extractRoot, Map<String, String> options)
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
		this.options = options;
	}

	public void perform() throws Exception
	{
		StringBuilder sb = new StringBuilder(100);
		sb.append("msiexec");
		sb.append(" /i");
		sb.append(" ");
		sb.append(fileName);
		sb.append(" /qn");
		sb.append(" /lcp c:\\log.log");
		sb.append(" INSTALLDIR=\"");
		sb.append(extractRoot);
		sb.append("\"");

		if (!(options == null))
		{
			for (String key : options.keySet())
			{
				sb.append(" ");
				sb.append(key);
				sb.append("=\"");
				sb.append(options.get(key));
				sb.append("\"");
			}
		}

		Process installProc = Runtime.getRuntime().exec(sb.toString());

		if (installProc.waitFor() != 0)
		{
			throw new Exception("There was some error while trying to install " + fileName + ".");
		}

		setChanged();
		notifyObservers(new ObserverEvent(getWorkerNr(), 100));
	}

	private String fileName;
	private String extractRoot;
	private Map<String, String> options;
}
