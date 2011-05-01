package geditor.repository;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.io.*;

import geditor.engine.operations.Operation;
import geditor.tools.Datetime;
import geditor.elements.GRootGroup;

/**
 * Default implementation of the {link Repository Repository} interface.
 * <br /><br />
 * Date: Feb 25, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class RepositoryImpl extends UnicastRemoteObject implements Repository
{
	/**
	 * Builds a new Repository object which will use <code>path</code> to read/store its files.
	 * <br /><br />
	 * @param path the path to use for reading/writing repository files
	 */
	public RepositoryImpl(String path) throws RemoteException
	{
		if (path.charAt(path.length() - 1) == File.separatorChar)
		{
			repositoryPath = path;
		}
		else
		{
			repositoryPath = path + File.separatorChar;
		}

		File repositoryDir = new File(repositoryPath);
		File versionFile = new File(repositoryPath + "version");

		System.out.println("Starting initialization of the repository...");

		// check to see if the directory specified in the configuration file (the one received as
		// parameter) exists
		if (repositoryDir.isDirectory())
		{
			System.out.println("Specified repository directory exists. Looking for version file.");

			// check to see if the version file exists in the directory
			if (versionFile.isFile())
			{
				System.out.println("Version file exists. Trying to read version from file.");

				try
				{
					ObjectInputStream vfReader =
						new ObjectInputStream(new FileInputStream(versionFile));
					currentVersion = vfReader.readInt();

					System.out.println("Version number (" + currentVersion + ") successfully " +
						"read from file.");

					vfReader.close();
				}
				catch (IOException e)
				{
					e.printStackTrace();
					System.out.println("Failed to read version number from version file. " +
						"Aborting.");
					System.exit(1);
				}
			}
			else
			{
				// create the version file and write 0 to it
				System.out.println("Version file missing. Assuming repository is empty. Initializing " +
					"version with 0 and trying to write new version file.");

				currentVersion = 0;
				try
				{
					// try to create the version file
					if (!versionFile.createNewFile())
					{
						System.out.println("Failed to create version file (" +
							versionFile.getAbsolutePath() + "). Aborting.");
						System.exit(1);
					}

					System.out.println("Version file successfully created.");

					ObjectOutputStream vfWriter =
						new ObjectOutputStream(new FileOutputStream(versionFile));
					vfWriter.writeInt(currentVersion);
					vfWriter.close();

					System.out.println("Version file successfully written with version 0.");
				}
				catch (IOException e)
				{
					e.printStackTrace();
					System.out.println("Failed to create version file (" +
						versionFile.getAbsolutePath() + ") or failed to write to it. " +
						"Aborting.");
					System.exit(1);
				}
			}
		}
		else
		{
			System.out.println("Specified repository directory missing. Trying to create it.");

			// try to create the repository directory
			if (!repositoryDir.mkdirs())
			{
				System.out.println("Failed to create repository directory (" +
						repositoryDir.getAbsolutePath() + "). Aborting.");
				System.exit(1);
			}

			System.out.println("Repository directory successfully created.");

			// create the version file and write 0 to it
			System.out.println("Initializing version with 0 and trying to write new version file.");

			currentVersion = 0;
			try
			{
				// try to create the version file
				if (!versionFile.createNewFile())
				{
					System.out.println("Failed to create version file (" +
						versionFile.getAbsolutePath() + "). Aborting.");
					System.exit(1);
				}

				System.out.println("Version file successfully created.");

				ObjectOutputStream vfWriter =
					new ObjectOutputStream(new FileOutputStream(versionFile));
				vfWriter.writeInt(currentVersion);
				vfWriter.close();

				System.out.println("Version file successfully written with version 0.");
			}
			catch (IOException e)
			{
				e.printStackTrace();
				System.out.println("Failed to create version file (" +
					versionFile.getAbsolutePath() + ") or failed to write to it. " +
					"Aborting.");
				System.exit(1);
			}
		}

		timestamps = new ArrayList(currentVersion);
		System.out.print("Reading timestamps into memory... ");

		// i starts from 1 because the delta between version(n) and version(n+1) is stored in a file called
		// v<n+1>
		for (int i = 1; i <= currentVersion; i++)
		{
			String fileName = repositoryPath + "v" + (new Integer(i)).toString();
			File file = new File(fileName);

			// check that file exists
			if (!file.isFile())
			{
				System.out.println("");
				System.out.println("Error: version file " + file.getAbsolutePath() + " doesn't exist.");
				System.out.println("Please create the *empty* file called " + file.getAbsolutePath() +
					" and restart the application.");
				System.out.println("Warning: the effect of this is that all operations changing from " +
					"version " + (i - 1) + " to version " + i + " are lost and all versions " +
					"starting from version " + i + " (inclusive) might be corrupt.");
				System.out.println("If you wish to reset the repository, please delete the " +
					"\"version\" file from the repository directory, i.e. " +
					versionFile.getAbsolutePath());
				System.out.println("Aborting.");
				System.exit(1);
			}

			Datetime timestamp = getTimestampFromFile(file);

			if (timestamp == null)
			{
				System.out.println();
				System.out.println("File " + file.getAbsolutePath() + " contains an invalid timestamp" +
					" entry (or doesn't have one at all). Aborting.");
				System.exit(1);
			}

			timestamps.add(timestamp);
		}

		System.out.println("done.");
	}

	/**
	 * {@inheritDoc}
	 */
	synchronized public boolean commit(GRootGroup currentState, int referenceVersion)
		throws RemoteException, VersionException, RepositoryException
	{
		// check that referenceVersion is a valid version
		if ((referenceVersion < 0) || (referenceVersion > currentVersion))
		{
			throw new VersionException("The reference version supplied at commit (" + referenceVersion +
				") was either negative or greater than the current version on the repository.");
		}

		// tell the client that an update is needed before he can commit
		if (referenceVersion != currentVersion)
		{
			return false;
		}

		try
		{
			// the delta between version(n) and version(n+1) is stored in the file called v<n+1>
			File vFile = new File(repositoryPath + "v" + (new Integer(currentVersion + 1)).toString());
			ObjectOutputStream vfWriter = new ObjectOutputStream(new BufferedOutputStream(
				new FileOutputStream(vFile)));

			Datetime timestamp = new Datetime();
			vfWriter.writeObject(timestamp);
			vfWriter.writeObject(currentState);
			vfWriter.close();

			timestamps.add(timestamp);
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.out.println("Failed to write operations to file during commit.");

			throw new RepositoryException("The repository failed to commit your version. Please notify " +
				"the administrator.");
		}

		// if we got this far, it means all went ok => we can increment currentVersion
		File versionFile = new File(repositoryPath + "version");

		System.out.println("Trying to update version file after a successful commit.");

		try
		{
			ObjectOutputStream vfWriter =
				new ObjectOutputStream(new FileOutputStream(versionFile));
			vfWriter.writeInt(currentVersion + 1);
			vfWriter.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.out.println("Failed to update version file.");

			throw new RepositoryException("The repository failed to commit your version. Please notify " +
				"the administrator.");
		}

		System.out.println("Version file successfully updated.");

		currentVersion++;

		return true;
	}

	/**
	 * {@inheritDoc}
	 */
	public GRootGroup getState(int version) throws RemoteException, VersionException, RepositoryException
	{
		if (version < 0)
		{
			throw new VersionException("version (" + version + ") was negative.");
		}

		if (version > currentVersion)
		{
			throw new VersionException("version (" + version + ") was greater than " +
				"currentVersion (" + currentVersion + ").");
		}

		// if we got this far, we know that 0 <= version <= currentVersion
		File vFile = new File(repositoryPath + "v" + (new Integer(version)).toString());

		try
		{
			ObjectInputStream opFile = new ObjectInputStream(new BufferedInputStream(new FileInputStream(vFile)));

			// jump over the timestamp
			opFile.readObject();

			GRootGroup result = (GRootGroup) opFile.readObject();

			return result;
		}
		catch (FileNotFoundException e)
		{
			System.out.println("File " + vFile.getAbsolutePath() + " doesn't exist.");
			throw new RepositoryException("The repository seems to be corrupt. Please notify " +
				"the administrator.");
		}
		catch (IOException e)
		{
			System.out.println("File " + vFile.getAbsolutePath() + " could not be read.");
			throw new RepositoryException("The repository seems to be corrupt. Please notify " +
				"the administrator.");
		}
		catch (ClassNotFoundException e)
		{
			throw new RepositoryException("The repository seems to be corrupt. Please notify " +
				"the administrator.");
		}
	}

	/**
	 * Returns the timestamp stored in the file <code>versionFile</code>. If the file doesn't exist or if it
	 * contains unexpected data, null is returned.
	 * <br /><br />
	 * @param versionFile the file to read the timestamp from
	 * @return a Datetime containing the timestamp read
	 */
	private Datetime getTimestampFromFile(File versionFile)
	{
		try
		{
			ObjectInputStream opFile = new ObjectInputStream(new BufferedInputStream(
				new FileInputStream(versionFile)));

			Datetime datetime = (Datetime) opFile.readObject();

			opFile.close();

			return datetime;
		}
		catch (IOException e)
		{
			e.printStackTrace();
			return null;
		}
		catch (ClassNotFoundException e)
		{
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * {@inheritDoc}
	 */
	synchronized public int getCurrentVersion() throws RemoteException
	{
		return currentVersion;
	}

	/**
	 * {@inheritDoc}
	 */
	public int getVersionByTimestamp(Datetime timestamp) throws RemoteException
	{
		for (int i = 0; i < timestamps.size(); i++)
		{
			Datetime ts = (Datetime) timestamps.get(i);
			if (ts.higherThan(timestamp))
			{
				return i+1;
			}
		}

		return -1;
	}

	/**
	 * The current version of the repository.
	 */
	private int currentVersion;

	/**
	 * The path to where the repository files are (to be) kept.
	 */
	private String repositoryPath;

	/**
	 * The list of timestamps associated with the version updates. timestamps.get(n) is the timestamp for version
	 * n+1.
	 */
	private ArrayList timestamps;
}
