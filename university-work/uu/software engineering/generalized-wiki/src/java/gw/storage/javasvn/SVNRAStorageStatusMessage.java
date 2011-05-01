package gw.storage.javasvn;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import gw.storage.StorageStatusMessage;
import gw.storage.StorageException;

import org.tmatesoft.svn.core.internal.util.SVNTimeUtil;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNProperty;

/**
 * This class is passed by the SVNRAStorage class when the status
 * is requested. It serves as a wrapper for the status information
 * of a file or directory. 
 * 
 * @see gw.storage.javasvn.SVNRAStorage#getStatus(String)
 * @see gw.storage.StorageStatusMessage
 * @author gjsmedin
 *
 */
public class SVNRAStorageStatusMessage extends StorageStatusMessage
{
	/**
	 * The repository's revision number to inspect. We could be inspecting
	 * an earlier version.
	 */
	private long _revision;
	
	/**
	 * If the requested file or directory was edited in this session,
	 * we have to get the most recent information from this.
	 * If it was not edited, this is null.
	 */
	private CommitItem _item;
	
	private String _lastAuthor;
	private long _lastRevision;
	private Date _lastDate;
	private boolean _isFileInRepo;
	private PathTree _workingcopy;

	/**
	 * This constructor creates an instance of this class
	 * with a SVNRepository and a revision number.
	 * @param repository The repository which path's status will be represented by this class.
	 * @param revision The revision of the supplied repository to be used to get the status.
	 * @param workingcopy The items that have been changed but not yet commited, i.e. the virtual working copy.
	 * @param path The path of the file or directory that will be inspected.
	 */
	public SVNRAStorageStatusMessage(
			String path,
			RepositoryCache repository,
			PathTree workingcopy) throws StorageException
	{
		try
		{
			// get the item from the container if it's there
			_item = workingcopy.getNodeChanges(path);
		}
		catch ( NodeIsFileException e )
		{
			throw new StorageException("Path " + path + " is invalid!");
		}

		_revision = workingcopy.getRevision();

		_isFileInRepo = workingcopy.hasFileNode(path);
		
		_workingcopy = workingcopy;
		
		// set properties
		HashMap<String, String> properties = getProperties(repository, path);
		_lastAuthor = (String) properties.get(SVNProperty.LAST_AUTHOR);
		_lastDate = SVNTimeUtil.parseDate(properties.get(SVNProperty.COMMITTED_DATE));
		_lastRevision = Long.parseLong((String) properties.get(SVNProperty.COMMITTED_REVISION));
	}
	
	/**
	 * Gets the properties from the server, or makes them up if 
	 * the file does not exist on the server.
	 * @param repository The repository to retrieve the path from
	 * @param path The path to get the properties from
	 * @return A map of properties with at least the svn:author,
	 * svn:committed-revision and svn:committed-date (note that
	 * these keys propably are different, use
	 * {@link SVNProperty} to be sure).  
	 * @throws StorageException exceptions forwarded from {@link SVNRepository}
	 */
	private HashMap<String, String> getProperties(RepositoryCache repository, String path) throws StorageException
	{
		HashMap<String, String> properties =
			new HashMap<String, String>();
		
		try
		{
			if ( !isAdded() )
			{ // get info from the server
				if ( isCopied() )
					path = _item.getCopyFromPath();
				else
					path = Path.removeSlashes(path);
			
				if ( _isFileInRepo )
					repository.getFile(_workingcopy.getRealPath(path), _revision, properties, null);
				else //FIXME: if this gets it's stuff recursively, this is SLOW ....
					repository.getDir(_workingcopy.getRealPath(path), _revision, properties, (Collection) null);
			}
			else
			{ // set properties for a newly added file
				properties.put(SVNProperty.LAST_AUTHOR, null);
				properties.put(SVNProperty.COMMITTED_DATE, null);
				properties.put(SVNProperty.COMMITTED_REVISION, "-1");
			}
		}
		catch ( Exception e )
		{
			throw new StorageException(e.getMessage());
		}
		
		return properties;
	}
	
	public Date getLastChangedDate()
	{
		if ( _lastDate.getTime() == 0 )
			return null;
			
		return _lastDate;
	}

	public long getLastChangedRevision()
	{
		return _lastRevision;
	}

	public String getLastCommitAuthor()
	{		
		return _lastAuthor;
	}

	public long getRevision()
	{
		if ( isAdded() )
			return 0;
		
		return _revision;
	}

	public boolean isAdded()
	{
		if ( _item == null )
			return false;
		
		return _item.isAddDirectory() || _item.isAddFile();
	}

	public boolean isCopied()
	{
		if ( _item == null )
			return false;
		
		return _item.isCopy();
	}

	public boolean isDeleted()
	{
		if ( _item == null )
			return false;
		
		return _item.isDelete();
	}

	public boolean isModified()
	{
		if ( _item == null )
			return false;
			
		return _item.isModFile();
	}

	public boolean isDirectory()
	{
		if ( _item == null )
			return !_isFileInRepo;
		
		return _item.isDirectory();
	}

	public boolean isFile()
	{
		if ( _item == null )
			return _isFileInRepo;
		
		return _item.isFile();
	}
}
