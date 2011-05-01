package gw.storage.javasvn.virtualwc;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNProperty;
import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import gw.storage.Storage;
import gw.storage.StorageBlameLine;
import gw.storage.StorageDirEntry;
import gw.storage.StorageException;
import gw.storage.StorageLogMessage;
import gw.storage.StorageStatusMessage;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.PathException;
import gw.storage.javasvn.pathtree.NodeNotFoundException;
import gw.storage.javasvn.pathtree.PathNode;
import gw.storage.javasvn.pathtree.RootNode;

public class VirtualWCStorage implements Storage
{
	private RootNode root;
	
	public VirtualWCStorage(String repositoryURL, String username, String password) throws StorageException
	{
        DAVRepositoryFactory.setup();
        SVNRepositoryFactoryImpl.setup();
        
        SVNRepository repository;
        long revision;
        
		try
		{
			repository = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(repositoryURL));

	        ISVNAuthenticationManager authManager =
	            SVNWCUtil.createDefaultAuthenticationManager(username, password);
	        repository.setAuthenticationManager(authManager);
	        
			revision = repository.getLatestRevision();
		}
		catch (SVNException e)
		{
			throw new StorageException(e);
		}

        root = new RootNode(repository, revision);
	}
	
	private PathNode getNode(String path) throws StorageException
	{
		try
		{
			return root.getNode(new Path(path));
		}
		catch (NodeNotFoundException e)
		{
			throw new PathException(path, root.getRevision());
		}
	}

	public Iterator<StorageBlameLine> blame(String path, long revisionStart,
			long revisionEnd) throws StorageException
	{
		// TODO This can be copied straight from javasvn.SVNRAStorage
		return null;
	}

	public OutputStream storeFile(String path) throws StorageException
	{
		System.out.print("storeFile(" + path);
		AddManager am = new AddManager(root);
		
		// TODO: delete the temporary files sometime too
		// maybe that should be done in the NodeData's finalizer?
		File file;
		
		try
		{
			file = File.createTempFile(".virtualWCStorage", ".tmp");
			System.out.print(")\n");
			return new StoreFileOutputStream(file, new Path(path), am);
		}
		catch (IOException e)
		{
			throw new StorageException(e);
		}	
	}

	public InputStream getFile(String path) throws StorageException
	{
		System.out.print("getFile(" + path);
		InputStream io = getNode(path).getData().getFile();
		System.out.print(")\n");
		return io;
	}

	public InputStream getFile(String path, long revision)
			throws StorageException
	{
		// TODO This obviously is a fake implementation
		// it should get the file of a specific revision
		
		return getFile(path);
	}

	public Iterator<String> getDirListing(String path) throws StorageException
	{
		return getDirListing(path, false);
	}

	public Iterator<String> getDirListing(String path, boolean recurse)
			throws StorageException
	{
		System.out.print("getDirListing(" + path + ", " + recurse);
		PathNode node = getNode(path);
		
		System.out.print(")\n");
		return getDirListingSet(node, recurse).iterator();
	}
	
	private Set<String> getDirListingSet(PathNode node, boolean recurse) throws StorageException
	{
		HashSet<String> dirListing = new HashSet<String>();
		
		if ( !node.isDir() )
			return dirListing;
		
		for ( PathNode child : node.getChildren() )
		{
			dirListing.add(child.getFullPath().toString());
			
			if ( recurse && child.isDir() )
				dirListing.addAll(getDirListingSet(child, true));
		}
		
		return dirListing;
	}

	public Map<String, StorageDirEntry> getDirListing(String path,
			long revision, boolean recurse) throws StorageException
	{
		// TODO We're cheating here since we don't care about
		// the revision number, that should someday be implemented
		// too of course.
		System.out.print("getDirListing(" + path + ", " + revision + ", " + recurse);
		Map<String, StorageDirEntry> dirlisting =
			getDirListingFromNode(getNode(path), recurse);
		System.out.print(")\n");
		
		return dirlisting;
	}
	
	private Map<String, StorageDirEntry> getDirListingFromNode(PathNode node,
			boolean recurse) throws StorageException
	{
		HashMap<String, StorageDirEntry> dirListing =
			new HashMap<String, StorageDirEntry>();
		
		if ( !node.isDir() )
			return dirListing;
		
		for ( PathNode child : node.getChildren() )
		{
			dirListing.put(child.getFullPath().toString(),
					new VirtualWCStorageDirEntry(child));
			
			if ( recurse && child.isDir() )
				dirListing.putAll(getDirListingFromNode(child, true));
		}
		
		return dirListing;
	}

	public Map<String, StorageStatusMessage> getStatus(String path)
			throws StorageException
	{
		return getStatus(path, false);
	}

	public Map<String, StorageStatusMessage> getStatus(String path,
			boolean recursive) throws StorageException
	{
		System.out.print("getStatus(" + path + ", " + recursive);
		Map<String, StorageStatusMessage> status =
				getStatusFromNode(getNode(path), recursive);
		System.out.print(")\n");
		return status;
	}
	
	private Map<String, StorageStatusMessage> getStatusFromNode(PathNode node,
			boolean recurse) throws StorageException
	{
		HashMap<String, StorageStatusMessage> status =
			new HashMap<String, StorageStatusMessage>();
		
		status.put(node.getFullPath().toString(),
				new VirtualWCStorageStatusMessage(node));
		
		if ( !node.isDir() )
			return status;
		
		for ( PathNode child : node.getChildren() )
		{
			status.putAll(getStatusFromNode(child, true));
		}
		
		return status;
	}

	public boolean fileExists(String path) throws StorageException
	{
		System.out.print("fileExists(");
		try
		{
			root.getNode(new Path(path));
			System.out.println(")");
			return true;
		}
		catch (NodeNotFoundException e)
		{
			return false;
		}
		
	}

	public boolean isDirectory(String path) throws StorageException
	{
		System.out.println("isDirectory()");
		// TODO: The javahl implementation also returned false
		// if the file didn't exist. But it should probably be
		// changed. BTW. I changed the comment in the Storage
		// interface
		try
		{
			return root.getNode(new Path(path)).isDir();
		}
		catch (NodeNotFoundException e)
		{
			return false;
		}
	}

	public void makeDirectory(String pathName) throws StorageException
	{
		System.out.println("makeDirectory()");
		// FIXME: ensurePathExists() makes me think,
		// AddManager.makeDirectory() will also ensure that the path
		// exists, so maybe that's wrong and this one should just
		// fail if the parent path does not exist?
		AddManager ad = new AddManager(root);
		
		ad.makeDirectory(new Path(pathName));
	}

	public void ensurePathExists(String path) throws StorageException
	{
		// FIXME: see makeDirectory()
		makeDirectory(path);
	}

	public void moveFile(String oldPath, String newPath, boolean force)
			throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public void copyFile(String originalPath, String copiedPath)
			throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public void deleteFile(String path, boolean force) throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public void revertFile(String path) throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public void revertFile(String path, long revision) throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public String getFileDiff(String path, long revision1, long revision2)
			throws StorageException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public SortedSet<StorageLogMessage> getLog(String path)
			throws StorageException
	{
		// TODO: this can be copied literally from javasvn.SVNRAStorage
		return null;
	}

	public void setProperty(String path, String property, String value,
			boolean recurse) throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public Map<String, String> getProperties(String path)
			throws StorageException
	{
		System.out.print("getProperties(" + path);
		PathNode node = getNode(path);
		Map<String, String> properties = 
			new HashMap<String, String>(node.getData().getProperties());
		
        // remove svn properties
		Iterator<String> propIt = properties.keySet().iterator();

		while ( propIt.hasNext() )
		{
			String key = propIt.next();
			if ( SVNProperty.isEntryProperty(key) || SVNProperty.isSVNProperty(key) )
			{
				propIt.remove();
				properties.remove(key);
			}
		}
		System.out.print(")\n");
		return properties;
	}

	public boolean commit(String message) throws StorageException
	{
		// TODO Auto-generated method stub
		return false;
	}

	public void update() throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public Iterator<String> getConflicts() throws StorageException
	{
		// TODO Auto-generated method stub
		return null;
	}

	public void setResolved(String path) throws StorageException
	{
		// TODO Auto-generated method stub

	}

	public void beginTransaction() throws StorageException
	{
		// TODO remove this function from the interface

	}

	public void cancelTransaction() throws StorageException
	{
		// TODO remove this function from the interface

	}

	public void endTransaction(String message) throws StorageException
	{
		// TODO remove this function from the interface

	}

	public void setUsername(String username) throws StorageException
	{
		// TODO Auto-generated method stub
	}

}
