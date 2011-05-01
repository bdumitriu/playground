package gw.storage.javasvn.virtualwc;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.PathExistsException;
import gw.storage.javasvn.pathtree.NodeNotFoundException;
import gw.storage.javasvn.pathtree.PathNode;
import gw.storage.javasvn.pathtree.ProxyNodeData;
import gw.storage.javasvn.pathtree.ProxyPathNode;

import java.io.OutputStream;

import org.tmatesoft.svn.core.SVNCommitInfo;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.diff.SVNDiffWindow;

/**
 * This class is used by a ProxyPathNode to update it's entries
 * if ProxyPathNode.getChildren() is requested. It is then driven
 * by the svnjava library.
 * 
 * Because this is usually called when we're doing a dirListing or a
 * getStatus, it's very likely that we'll need the properties to
 * construct StorageDirEntries and StorageStatusMessages. Because
 * it's so much cheeper to get them now, than to get them one by one
 * we'll prefetch them here. 
 * 
 * FIXME:
 * Unfortunately the javasvn library doesn't seem to work properly:
 * if the recursive parameter is set to false in SVNRepository.checkout(..), it
 * will not return the directories within this directory, just the files.
 * Because of the workaround the newly created directory nodes do not have to be
 * ProxyPathNodes, normal PathNodes suffice, since all their subdirectories
 * will be retrieved anyway.
 * 
 * @see org.tmatesoft.svn.core.io.SVNRepository.checkout
 * @see ProxyPathNode
 * 
 * @author gideon
 *
 */
public class CheckoutEditor implements ISVNEditor
{
	private PathNode rootNode;
	private PathNode currentNode;
	private ProxyNodeData currentData;
	
    public CheckoutEditor(PathNode root)
    {
        this.rootNode = root;
        this.currentNode = null;
    }
    
    private void addChildToPath(PathNode child, Path parentPath) throws SVNException
    {
    	try
    	{
    		rootNode.getNode(parentPath).addChild(child);
		}
		catch (PathExistsException e)
		{
			// do nothing since we don't want to
			// replace existing children
			return;
		}
		catch (NodeNotFoundException e)
		{
			throw new SVNException(e);
		}
		catch (StorageException e)
		{
			throw new SVNException(e);
		}
    }

    public void addDir(String path, String copyFromPath, long copyFromRevision)
            throws SVNException
    {
    	Path parentPath = new Path(path);
		String newPath = parentPath.pop();
		//PathNode newNode = new ProxyPathNode(newPath, true);
		currentNode = new PathNode(newPath, true);
		currentData = new ProxyNodeData();
		
		currentNode.setData(currentData);
		addChildToPath(currentNode, parentPath);
    }    

    public void addFile(String path, String copyFromPath, long copyFromRevision)
            throws SVNException
    {
		Path parentPath = new Path(path);
		String newPath = parentPath.pop();
		currentNode = new PathNode(newPath, false);
		currentData = new ProxyNodeData();
		
		currentNode.setData(currentData);
		addChildToPath(currentNode, parentPath);
    }
    
    public void changeFileProperty(String path, String name, String value) throws SVNException
	{
    	// only set the properties of nodes that we created
    	if ( currentNode != null && currentData == currentNode.getData() )
    		currentData.addProperty(name, value);
	}
    
    public void changeDirProperty(String name, String value) throws SVNException
	{
    	// only set the properties of nodes that we created
    	if ( currentNode != null && currentData == currentNode.getData() )
    		currentData.addProperty(name, value);
	}
    
    //////////////////////////////////////////////////////////////////
	////   Unimplemented methods                                  ////
	//////////////////////////////////////////////////////////////////
    
    public OutputStream textDeltaChunk(String path, SVNDiffWindow diffWindow) throws SVNException
	{
    	return null;
	}

    public void targetRevision(long revision) throws SVNException {}

    public void openRoot(long revision) throws SVNException {}

    public void deleteEntry(String path, long revision) throws SVNException {}

    public void absentDir(String path) throws SVNException {}

    public void absentFile(String path) throws SVNException {}

    public void openDir(String path, long revision) throws SVNException {}

    public void closeDir() throws SVNException {}

    public void openFile(String path, long revision) throws SVNException {}

    public void applyTextDelta(String path, String baseChecksum)
            throws SVNException {}

    public void textDeltaEnd(String path) throws SVNException {}

    public void closeFile(String path, String textChecksum) throws SVNException {}

    public SVNCommitInfo closeEdit() throws SVNException
    {
        return null;
    }

    public void abortEdit() throws SVNException
    {
    	throw new SVNException("CheckoutEditor aborted!");
    }
}
