package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.NotADirException;

import java.util.Collection;


/**
 * This class is used for testing. It bypasses the ProxyPathNode grandparent
 * because we want to be able to check without using the repository.
 */
public class MockRootNode extends RootNode
{
	PathNode pathNode = new PathNode();

	@Override
	public PathNode getChild(String childPath) throws StorageException
	{
		return pathNode.getChild(childPath);
	}

	@Override
	public Collection<PathNode> getChildren() throws StorageException {
		return pathNode.getChildren();
	}

	@Override
	public void addChild(PathNode child) throws NotADirException, StorageException {
		pathNode.addChild(child);
	}

	@Override
	public NodeData getData() {
		return pathNode.getData();
	}

	@Override
	public Path getFullPath() {
		return pathNode.getFullPath();
	}

	@Override
	public PathNode getNode(Path path) throws NodeNotFoundException, StorageException {
		return pathNode.getNode(path);
	}

	@Override
	public String getPath() {
		return pathNode.getPath();
	}

	@Override
	public RootNode getRoot() {
		return pathNode.getRoot();
	}

	@Override
	public boolean hasChanges() {
		return pathNode.hasChanges();
	}

	@Override
	public boolean isDir() {
		return pathNode.isDir();
	}

	@Override
	public boolean isFile() {
		return pathNode.isFile();
	}

	@Override
	public boolean isLogicallyDeleted() {
		return pathNode.isLogicallyDeleted();
	}

	@Override
	protected void removeChild(String childPath) throws StorageException {
		pathNode.removeChild(childPath);
	}

}
