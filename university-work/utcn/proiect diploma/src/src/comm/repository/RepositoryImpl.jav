package comm.repository;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.dom.DOMSource;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.io.*;

import core.Operation;
import data.Datetime;

/**
 * Default implementation of the {link Repository Repository} interface.
 * <br /><br />
 * TODO: create DTD's for the xml files used by the repository and make all parsing validating
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
		// v<n+1>.xml
		for (int i = 1; i <= currentVersion; i++)
		{
			String fileName = repositoryPath + "v" + (new Integer(i)).toString() + ".xml";
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
	/*
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

		try
		{
			File vFile = new File(repositoryPath + "v" + (new Integer(currentVersion + 1)).toString());
			ObjectOutputStream vfWriter = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(vFile)));

			for (int i = 0; i < operations.size(); i++)
			{
				vfWriter.writeObject(operations.get(i));
			}

			vfWriter.close();
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
	*/

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

		Datetime timestamp = new Datetime();

		try
		{
			// the delta between version(n) and version(n+1) is stored in the file called v<n+1>.xml
			File vFile = new File(repositoryPath + "v" + (new Integer(currentVersion + 1)).toString() +
				".xml");

			// create the Document
			Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();

			// fill Document with data
			Element root = doc.createElement("version-update");
			doc.appendChild(root);

			root.appendChild(doc.createTextNode("\n\t"));
			root.appendChild(timestamp.toDocumentElement(doc));

			for (int i = 0; i < operations.size(); i++)
			{
				root.appendChild(doc.createTextNode("\n\t"));
				root.appendChild(((Operation) operations.get(i)).toDocumentElement(doc));
			}

			root.appendChild(doc.createTextNode("\n"));

			DOMSource source = new DOMSource(doc);
			StreamResult result = new StreamResult(vFile);

			TransformerFactory.newInstance().newTransformer().transform(source, result);
		}
		catch (ParserConfigurationException e)
		{
			e.printStackTrace();
			System.out.println("Failed to create XML document during commit.");

			throw new RepositoryException("The repository failed to commit your version. Please notify " +
				"the administrator.");
		}
		catch (TransformerException e)
		{
			e.printStackTrace();
			System.out.println("Failed to transform XML document to stream.");

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

		// if we got here, it means we can also add the timestamp to the list of timestamps
		timestamps.add(timestamp);

		for (int i = 0; i < timestamps.size(); i++)
		{
			Datetime datetime = (Datetime) timestamps.get(i);
			System.out.println(datetime);
		}

		return true;
	}

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
		// in a file called v<n+1>.xml
		for (int i = fromVersion + 1; i <= toVersion; i++)
		{
			File vFile = new File(repositoryPath + "v" + (new Integer(i)).toString() + ".xml");

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
	 * Returns the timestamp stored in the file <code>versionFile</code>. If the file doesn't exist or if it
	 * contains unexpected data, null is returned.
	 * <br /><br />
	 * @param versionFile the file to read the timestamp from
	 * @return a Datetime containing the timestamp read
	 */
	private Datetime getTimestampFromFile(File versionFile)
	{
		Datetime timestamp = null;
		try
		{
			Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(versionFile);

			// this returns the element for the "operations" tag
			Node root = doc.getFirstChild();

			// this returns the list of elements of type "operation" interspersed with TextNode's and
			// preceded by an element of type "timestamp"
			NodeList children = root.getChildNodes();

			for (int i = 0; i < children.getLength(); i++)
			{
				Node currentChild = children.item(i);

				// only look for the timestamp element
				if ((currentChild.getNodeType() == Node.ELEMENT_NODE) &&
					(currentChild.getNodeName() == "timestamp"))
				{
					timestamp = new Datetime();
					if (!timestamp.fromDocumentElement(currentChild))
					{
						return null;
					}

					i = children.getLength();
				}
			}
		}
		catch (SAXException e)
		{
			e.printStackTrace();
			return null;
		}
		catch (IOException e)
		{
			e.printStackTrace();
			return null;
		}
		catch (ParserConfigurationException e)
		{
			e.printStackTrace();
			return null;
		}

		return timestamp;
	}

	/**
	 * Returns the operations stored in the file <code>versionFile</code> as an ArrayList of
	 * {@link core.Operation Operation}'s. If the file doesn't exist or if it contains unexpected data, null is
	 * returned.
	 * <br /><br />
	 * @param versionFile the file containing the operations
	 * @return an ArrayList containing all {@link core.Operation Operation}'s from <code>versionFile</code>
	 */
	private ArrayList getOperationsFromFile(File versionFile)
	{
		ArrayList result = new ArrayList();
		try
		{
			Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(versionFile);

			// this returns the element for the "operations" tag
			Node root = doc.getFirstChild();

			// this returns the list of elements of type "operation" interspersed with TextNode's and
			// preceded by an element of type "timestamp"
			NodeList ops = root.getChildNodes();

			for (int i = 0; i < ops.getLength(); i++)
			{
				Node currentOp = ops.item(i);

				// igonore TextNode's (they only contain whitespace)
				if ((currentOp.getNodeType() == Node.ELEMENT_NODE) &&
					(currentOp.getNodeName() == "operation"))
				{
					Operation op = new Operation();
					if (!op.fromDocumentElement(currentOp))
					{
						return null;
					}
					else
					{
						result.add(op);
					}
				}
			}
		}
		catch (SAXException e)
		{
			e.printStackTrace();
			return null;
		}
		catch (IOException e)
		{
			e.printStackTrace();
			return null;
		}
		catch (ParserConfigurationException e)
		{
			e.printStackTrace();
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
	 * n+1 (i.e. the timestamp from file v<n+1>.xml).
	 */
	private ArrayList timestamps;
}
