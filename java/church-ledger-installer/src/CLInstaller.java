
import java.util.Observer;
import java.util.Observable;
import java.util.Properties;
import java.util.HashMap;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;
import java.io.FileInputStream;
import java.io.BufferedInputStream;
import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Jan 9, 2005
 * Time: 11:07:09 PM
 * To change this template use File | Settings | File Templates.
 */
public class CLInstaller
{
	public static String configFile = "config/config.xml";
	public static Properties config;

	public static void main(String[] args)
	{
		CLInstaller cli = new CLInstaller();

		cli.initProperties();
		cli.runGUIInstaller();
	}

	public void runGUIInstaller()
	{
		GUIInstaller gui = new GUIInstaller(this);
		gui.install();
	}

	public void runTextInstaller()
	{
		System.out.print("Install to: ");
		StringBuffer str = new StringBuffer();
		try
		{
			char c;
			while (System.getProperty("line.separator").indexOf(c = (char) System.in.read()) == -1)
			{
				str.append(c);
			}

			System.in.skip(System.getProperty("line.separator").length() - 1);
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		initTextWorkers(str.toString());
		Thread workerThread = new Thread(new WorkerThread(workers));
		workerThread.start();
		try
		{
			workerThread.join();
		}
		catch (InterruptedException e)
		{
			System.exit(1);
		}
	}

	public void initProperties()
	{
		config = new Properties();

		try
		{
			config.loadFromXML(new BufferedInputStream(new FileInputStream(configFile)));
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			System.out.println("Could not find the configuration file: " + configFile + ".");
			System.exit(1);
		}
	}

	public void initTextWorkers(String extractRoot)
	{
		initWorkers(extractRoot);

		workers[0].addObserver(new TextObserver(workers[0].getDescription()));
	}

	public Worker[] getWorkers()
	{
		return workers;
	}

	public void initWorkers(String extractRoot)
	{
		workers = new Worker[4];

		if (!(extractRoot.endsWith(System.getProperty("file.separator"))))
		{
			extractRoot += System.getProperty("file.separator");
		}

		String base = config.getProperty("archivedir");
		if (base == null)
		{
			System.out.println("The \"archivedir\" property is not set in the configuration file.");
		}
		if (!(base.endsWith(System.getProperty("file.separator"))))
		{
			base += System.getProperty("file.separator");
		}

		String fileName = config.getProperty("mysql.zip");
		if (fileName == null)
		{
			System.out.println("The \"mysql.zip\" property is not set in the configuration file.");
			System.exit(1);
		}

		workers[0] = new UnpackerWorker(base + fileName, extractRoot);
		workers[0].setDescription("Installing MySQL server... ");
		workers[0].setWorkerNr(0);

		// get the name of the directory where mysql was installed
		try
		{
			ZipFile zipFile = new ZipFile(base + fileName);
			ZipEntry ze = (ZipEntry) zipFile.entries().nextElement();
			workers[1] = new MySQLWorker(replaceFileSeparator(extractRoot + ze.getName()));
			workers[1].setDescription("Setting up MySQL server... ");
			workers[1].setWorkerNr(1);
		}
		catch (Exception e)
		{
			System.out.println("An unexpected error occured while trying to retrieve the path to MySQL.");
			System.exit(1);
		}

		fileName = config.getProperty("php.zip");
		if (fileName == null)
		{
			System.out.println("The \"php.zip\" property is not set in the configuration file.");
			System.exit(1);
		}

		workers[2] = new UnpackerWorker(base + fileName, extractRoot);
		workers[2].setDescription("Installing PHP... ");
		workers[2].setWorkerNr(2);
		//workers[1].setCallbackFrequency((byte) 10);

		HashMap<String, String> options = new HashMap<String, String>();
		options.put("SERVERDOMAIN", "test.org");
		options.put("SERVERNAME", "localhost.test.org");
		options.put("SERVERADMIN", "admin@test.org");
		options.put("ALLUSERS", "1");

		fileName = config.getProperty("apache.msi");
		if (fileName == null)
		{
			System.out.println("The \"apache.msi\" property is not set in the configuration file.");
			System.exit(1);
		}

		workers[3] = new MSIWorker(base + fileName, extractRoot, options);
		workers[3].setDescription("Installing Apache server... ");
		workers[3].setWorkerNr(3);
	}

	private String replaceFileSeparator(String s)
	{
		StringBuilder sb = new StringBuilder(s);
		for (int i = 0; i < sb.length(); i++)
		{
			if (sb.charAt(i) == '/')
			{
				sb.replace(i, i+1, "\\");
			}
		}
		return sb.toString();
	}

	private Worker[] workers;

	class TextObserver implements Observer
	{
		public TextObserver(String message)
		{
			this.message = message;
		}

		public void update(Observable o, Object arg)
		{
			System.out.println(message + ((ObserverEvent) arg).getPercentile() + "% completed.");
		}

		private String message;
	}
}
