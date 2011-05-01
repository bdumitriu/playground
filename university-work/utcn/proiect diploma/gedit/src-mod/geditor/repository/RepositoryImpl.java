package geditor.repository;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.io.*;

import geditor.engine.operations.Operation;
import geditor.tools.Datetime;

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
	// TODO: change version file to XML
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

		/*
		timestamps = new ArrayList(currentVersion);
		System.out.print("Reading timestamps into memory... ");

		File tsFile = new File(repositoryPath + "timestamps");
		ObjectInputStream fileReader = null;
		try
		{
			if (currentVersion != 0)
			{
				fileReader = new ObjectInputStream(new BufferedInputStream(new FileInputStream(tsFile)));
			}
		}
		catch (FileNotFoundException e)
		{
			System.out.println("\nError: timestamps file doesn't exist. Please reset the repository by " +
				"deleting the \"version\" file from the repository directory, i.e. " +
				tsFile.getAbsolutePath());
			System.exit(1);
		}
		catch (IOException e)
		{
			System.out.println("\nAn error occured while trying to open the timestamps file. Please check " +
				"its rights and try again.");
			System.exit(1);
		}

		try
		{
			for (int i = 0; i < currentVersion; i++)
			{
				Object timestamp = fileReader.readObject();

				if ((timestamp == null) || !(timestamp instanceof Datetime))
				{
					System.out.println("\nThe timestamps file contains erroneus entries. Please " +
						"reset the repository by deleting the \"version\" file from the " +
						" repository directory, i.e. " + tsFile.getAbsolutePath());
					System.exit(1);
				}

				timestamps.add(timestamp);
			}
		}
		catch (IOException e)
		{
			System.out.println("\nAn error occured while trying to read from timestamps file. Please " +
				"check its rights and try again.");
			System.exit(1);
		}
		catch (ClassNotFoundException e)
		{
			e.printStackTrace();
			System.exit(1);
		}
		*/

		System.out.println("done.");
	}

	/**
	 * {@inheritDoc}
	 */
	synchronized public boolean commit(ArrayList operations, int referenceVersion)
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

		/*
		Datetime timestamp = new Datetime();

		try
		{
			File file = new File(repositoryPath + "timestamps");
			ObjectOutputStream fileWriter = new ObjectOutputStream(new BufferedOutputStream(
				new FileOutputStream(file, true)));

			fileWriter.writeObject(timestamp);

			fileWriter.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.out.println("Failed to write timestamps file during commit.");

			throw new RepositoryException("The repository failed to commit your version. Please notify " +
				"the administrator.");
		}
		*/

		try
		{
			// the delta between version(n) and version(n+1) is stored in the file called v<n+1>
			File vFile = new File(repositoryPath + "v" + (new Integer(currentVersion + 1)).toString());
			ObjectOutputStream vfWriter = new ObjectOutputStream(new BufferedOutputStream(
				new FileOutputStream(vFile)));

			Datetime timestamp = new Datetime();
			vfWriter.writeObject(timestamp);
			vfWriter.writeObject(new Integer(operations.size()));

			for (int i = 0; i < operations.size(); i++)
			{
				vfWriter.writeObject(operations.get(i));
			}

			vfWriter.close();

			timestamps.add(timestamp);
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.out.println("Failed to write operations to file during commit.");

			/*
			restoreTimestampsFile();
			*/

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

			/*
			restoreTimestampsFile();
			*/

			throw new RepositoryException("The repository failed to commit your version. Please notify " +
				"the administrator.");
		}

		System.out.println("Version file successfully updated.");

		currentVersion++;

		// if we got here, it means we can also add the timestamp to the list of timestamps
		/*
		timestamps.add(timestamp);
		*/

		return true;
	}

	/**
	 * Deletes the last entry in the timestamp file.
	 */
	/*
	private void restoreTimestampsFile()
	{
		try
		{
			File inFile = new File(repositoryPath + "timestamps");
			File outFile = new File(repositoryPath + "timestamps.restore");

			ObjectInputStream fileReader = new ObjectInputStream(new BufferedInputStream(
				new FileInputStream(inFile)));
			ObjectOutputStream fileWriter = new ObjectOutputStream(new BufferedOutputStream(
				new FileOutputStream(outFile)));

			for (int i = 0; i < currentVersion; i++)
			{
				Object timestamp = fileReader.readObject();

				if ((timestamp == null) || !(timestamp instanceof Datetime))
				{
					System.out.println("The timestamps file contains erroneus entries. " +
						"The repository is inconsistent.");
				}
				else
				{
					fileWriter.writeObject(timestamp);
				}
			}

			fileReader.close();
			fileWriter.close();

			inFile.delete();
			outFile.renameTo(new File(repositoryPath + "timestamps"));
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.out.println("Failed to restore timestamps file. The repository is inconsistent.");
		}
		catch (ClassNotFoundException e)
		{
			e.printStackTrace();
			System.out.println("Failed to restore timestamps file. The repository is inconsistent.");
		}
	}
	*/

	/**
	 * {@inheritDoc}
	 */
	public ArrayList getOperations(int fromVersion, int toVersion)
		throws RemoteException, VersionException, RepositoryException
	{
		if (toVersion < fromVersion)
		{
			throw new VersionException("toVersion (" + toVersion + ") was lower than fromVersion (" +
				fromVersion + ").");
		}

		if (fromVersion < 0)
		{
			throw new VersionException("fromVersion (" + fromVersion + ") was negative.");
		}

		if (fromVersion > currentVersion)
		{
			throw new VersionException("fromVersion (" + fromVersion + ") was greater than " +
				"currentVersion (" + currentVersion + ").");
		}

		if (toVersion > currentVersion)
		{
			throw new VersionException("toVersion (" + toVersion + ") was greater than " +
				"currentVersion (" + currentVersion + ").");
		}

		// if we got this far, we know that 0 <= fromVersion <= toVersion <= currentVersion
		ArrayList result = new ArrayList();

		// i starts from (fromVersion + 1) because the delta between version(n) and version(n+1) is stored
		// in a file called v<n+1>
		for (int i = fromVersion + 1; i <= toVersion; i++)
		{
			File vFile = new File(repositoryPath + "v" + (new Integer(i)).toString());

			ArrayList temp = getOperationsFromFile(vFile);

			// this means there was some sort of error while reading the file
			if (temp == null)
			{
				System.out.println("File " + vFile.getAbsolutePath() + " doesn't exist or contains " +
					"invalid data.");
				throw new RepositoryException("The repository seems to be corrupt. Please notify " +
					"the administrator.");
			}

			result.addAll(temp);
		}

		return result;
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
	 * Returns the operations stored in the file <code>versionFile</code> as an ArrayList of
	 * {@link geditor.engine.operations.Operation Operation}'s. If the file doesn't exist or if it contains
	 * unexpected data, null is returned.
	 * <br /><br />
	 * @param versionFile the file containing the operations
	 * @return an ArrayList containing all {@link geditor.engine.operations.Operation Operation}'s from
	 *	<code>versionFile</code>
	 */
	private ArrayList getOperationsFromFile(File versionFile)
	{
		ArrayList result = new ArrayList();
		try
		{
			ObjectInputStream opFile = new ObjectInputStream(new BufferedInputStream(new FileInputStream(versionFile)));

			Object trash = opFile.readObject();
			Integer temp = (Integer) opFile.readObject();
			int nrOps = temp.intValue();

			for (int i = 0; i < nrOps; i++)
			{
				Object operation = opFile.readObject();

				if ((operation == null) || !(operation instanceof Operation))
				{
					return null;
				}
				else
				{
					result.add(operation);
				}
			}
		}
		catch (FileNotFoundException e)
		{
			return null;
		}
		catch (IOException e)
		{
			return null;
		}
		catch (ClassNotFoundException e)
		{
			return null;
		}

		return result;
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
