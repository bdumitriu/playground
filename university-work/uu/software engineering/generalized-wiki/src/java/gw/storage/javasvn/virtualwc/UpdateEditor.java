package gw.storage.javasvn.virtualwc;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.SVNRAWorkspaceMediator;
import gw.storage.javasvn.pathtree.NodeNotFoundException;
import gw.storage.javasvn.pathtree.PathNode;

import java.io.OutputStream;

import org.tmatesoft.svn.core.SVNCommitInfo;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.diff.SVNDiffWindow;

public class UpdateEditor implements ISVNEditor
{
	private PathNode root;
	private long targetRevision = -1;
	private SVNRAWorkspaceMediator mediator;
	
	public UpdateEditor(PathNode root)
	{
		this.root = root;
	}

	public void targetRevision(long revision) throws SVNException
	{
		targetRevision = revision;
	}

	public void openRoot(long revision) throws SVNException
	{
		mediator = new SVNRAWorkspaceMediator();
	}

	public void deleteEntry(String path, long revision) throws SVNException {}

	public void absentDir(String path) throws SVNException {}

	public void absentFile(String path) throws SVNException {}

	public void addDir(String path, String copyFromPath, long copyFromRevision)
			throws SVNException 
	{
    	try
    	{
    		Path parentPath = new Path(path);
    		String newPath = parentPath.pop();
    		PathNode parent = root.getNode(parentPath);
		
    		if ( parent.getChild(newPath) != null )
    		{
    			if ( !parent.getChild(newPath).isLogicallyDeleted() )
    			{
    				// insert conflict node
    			}
    		}

    		PathNode newNode = new PathNode(newPath, true);    		
			root.getNode(parentPath).addChild(newNode);
		}
    	catch (StorageException e)
    	{
			throw new SVNException(e);
		}
    	catch (NodeNotFoundException e)
    	{
			throw new SVNException(e);
		}
	}

	public void openDir(String path, long revision) throws SVNException
	{
		
	}

	public void changeDirProperty(String name, String value)
			throws SVNException
	{
		
	}

	public void closeDir() throws SVNException
	{
	
	}

	public void addFile(String path, String copyFromPath, long copyFromRevision)
			throws SVNException
	{
	
	}

	public void openFile(String path, long revision) throws SVNException
	{
	
	}

	public void applyTextDelta(String path, String baseChecksum)
			throws SVNException
	{
	
	}

	public OutputStream textDeltaChunk(String path, SVNDiffWindow diffWindow)
			throws SVNException
	{
		return null;
	}

	public void textDeltaEnd(String path) throws SVNException
	{
	
	}

	public void changeFileProperty(String path, String name, String value)
			throws SVNException
	{
	
	}

	public void closeFile(String path, String textChecksum) throws SVNException
	{
	
	}

	public SVNCommitInfo closeEdit() throws SVNException
	{
		return null;
	}

	public void abortEdit() throws SVNException
	{
	
	}

}
