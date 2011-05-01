package gw.storage.javasvn.virtualwc;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.NotAFileException;
import gw.storage.javasvn.exceptions.NotADirException;
import gw.storage.javasvn.pathtree.DirectoryChanges;
import gw.storage.javasvn.pathtree.FileChanges;
import gw.storage.javasvn.pathtree.NodeNotFoundException;
import gw.storage.javasvn.pathtree.PathNode;
import gw.storage.javasvn.pathtree.RootNode;

import java.io.File;

public class AddManager
{
	private RootNode root; 
	
	public AddManager(RootNode root)
	{
		this.root = root;
	}
	
	/**
	 * Creates a directory and, if needed, also parent directories.
	 * For example if we start out with an emty root '/' and we call
	 * this function with '/somedir/otherdir', both 'somedir' and
	 * 'otherdir' get created.
	 * @param path Directory to be created
	 * @throws StorageException This exception is thrown when the
	 * path already existed.
	 * @throws NotADirException This exception is thrown when you try
	 * to create a directory within a file.
	 */
	public void makeDirectory(Path path) throws StorageException, NotADirException
	{
		try
		{
			if ( root.getNode(path).isFile() )
				throw new NotADirException(path.toString());
		}
		catch ( NodeNotFoundException e )
		{
			makeDirectories(e.getLastNode(), e.getRemainingPath());
			return;
		}
		
		throw new StorageException("Path " + path + " already exists!");
	}
	
	/**
	 * Creates all directories given in a path starting from a specific node.
	 * It assumes 
	 * @param node the node to start creating from
	 * @param path path of directories to be created
	 * @return the last created directory
	 * @throws NotADirException This exception is thrown when you're trying
	 * to create a directory in or over a file.
	 * @throws StorageException 
	 */
	private PathNode makeDirectories(PathNode node, Path path) throws NotADirException, StorageException
	{
		PathNode current = node;
		
		for ( String dir : path.toArray() )
		{
			PathNode child;
			
			if ( current.isDir() )
				child = current.getChild(dir);
			else
				throw new NotADirException(current.getFullPath());
			
			if ( child == null )
				child = new PathNode(dir, true);
			
			DirectoryChanges.addDir(child);
			
			// the following throws an exception if current is not a dir
			current.addChild(child);
			
			current = child;
		}
		
		return current;
	}
	
	/**
	 * Creates or alters a file with contents and, if needed, also parent directories.
	 * For example if we start out with an emty root '/' and we call
	 * this function with '/somedir/otherdir/somefile', both 'somedir' and
	 * 'otherdir' get created.
	 * @param path File's path to be created or modified
	 * @param newFile The file's (new) contents
	 * @throws NotADirException This exception is thrown when you
	 * try to create a directory within a file.
	 * @throws NotAFileException This exception is thrown when you try to modify
	 * a directory.
	 * @throws StorageException Is thrown when the repository fails.
	 */
	public void storeFile(Path path, File newFile) throws NotADirException, NotAFileException, StorageException  
	{
		// ensure path exists
		PathNode current;
		Path remainingPath = new Path(path);
		String filename = remainingPath.pop();
		
		try
		{
			current = root.getNode(path);
		}
		catch ( NodeNotFoundException e )
		{
			remainingPath = e.getRemainingPath();
			remainingPath.pop();
			current = makeDirectories(e.getLastNode(), remainingPath);
			assert current.getChild(filename) == null;
			
			// we have to add the file	
			PathNode newNode = new PathNode(filename, false);
			FileChanges.addFile(newNode, newFile);
			current.addChild(newNode);
			return;
		}
		
		// we have to modify the file
		FileChanges.modifyFile(current, newFile);
	}
}
