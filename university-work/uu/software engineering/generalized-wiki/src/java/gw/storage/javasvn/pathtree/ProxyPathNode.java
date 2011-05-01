package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;
import gw.storage.javasvn.exceptions.NotADirException;
import gw.storage.javasvn.virtualwc.CheckoutEditor;

import java.util.Collection;
import java.util.HashSet;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNNodeKind;
import org.tmatesoft.svn.core.io.SVNRepository;

public class ProxyPathNode extends PathNode
{
	private boolean gotChildren;
	private HashSet<String> noChild = new HashSet<String>();
	
	protected ProxyPathNode()
	{
		super();
	}
	
	public ProxyPathNode(String path, boolean isDir)
	{
		super(path, isDir);
		gotChildren = false;
	}

	@Override
	public PathNode getChild(String childPath) throws StorageException
	{
		PathNode child = super.getChild(childPath);
		
		if ( child != null )
			return child;	
		
		if ( !isDir() || gotChildren )
			return null;
		
		// the same calls for nonexisting nodes are made often
		// so this is an optimization
		if ( noChild.contains(childPath) )
			return null;
		
		// couldn't do it locally so try it on the repository
		RootNode root = getRoot();
		
		try
		{ 
			String path = getFullPath().toString().substring(1);
			path += "/" + childPath;
			if ( path.startsWith("/") )
				path = path.substring(1);
			
			SVNRepository repository = root.getRepository();
			SVNNodeKind nodeKind = repository.checkPath(path, root.getRevision());
			
			if ( nodeKind == SVNNodeKind.NONE )
			{
				noChild.add(childPath);
				return null;
			}
			
			// create a new proxy storage node
			boolean isDir = (nodeKind == SVNNodeKind.DIR);
			child = new ProxyPathNode(childPath, isDir);
			child.setData(new ProxyNodeData());
			addChild(child);
		}
		catch (SVNException e)
		{
			throw new StorageException(e);
		}
			
		return child;
	}

	@Override
	public Collection<PathNode> getChildren() throws StorageException
	{
		if ( !isDir() )
			return null;
		
		if ( gotChildren )
			return super.getChildren();
		
		RootNode root = getRoot();
		
		SVNRepository repository = root.getRepository();
		long revision = root.getRevision();
		String checkoutPath = getFullPath().toString().substring(1);
		
		try
		{
			CheckoutEditor editor = new CheckoutEditor(root);
			
			// FIXME: unfortunately the javasvn library doesn't seem to work
			// properly: if recursive is set to false, it will not return
			// the directories within this directory, just the files
			// if you change this, also take a look at the checkouteditor
			repository.checkout(revision, checkoutPath, true, editor);
		}
		catch ( SVNException e )
		{
			throw new StorageException(e);
		}
		
		gotChildren = true;
		noChild = null;
		return super.getChildren();
	}

	@Override
	public void addChild(PathNode child) throws NotADirException, StorageException
	{
		if ( noChild != null )
			noChild.remove(child.getPath());
			
		super.addChild(child);
	}
}
