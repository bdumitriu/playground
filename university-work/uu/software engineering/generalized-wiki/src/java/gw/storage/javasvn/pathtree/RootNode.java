package gw.storage.javasvn.pathtree;

import org.tmatesoft.svn.core.io.SVNRepository;

public class RootNode extends ProxyPathNode
{
	private long revision = 0;
	private SVNRepository repository = null;

	protected RootNode()
	{
		super();
	}
	
	public RootNode(SVNRepository repository, long revision)
	{
		super();
		this.repository = repository;
		this.revision = revision;
		setData(new ProxyNodeData());
	}
	
	public long getRevision() { return revision; }

	public SVNRepository getRepository() { return repository; }

//	@Override
//	public void setData(NodeData newData)
//	{
//		// we don't allow data in the root!
//		assert false;
//	}

	@Override
	protected void setParent(PathNode parent)
	{
		// we don't allow a root to have a parent!
		assert false;
	}

	@Override
	public PathNode getParent()
	{
		// overriden just to be sure
		// TODO: maybe even throw an exception?
		return null;
	}
	
}
