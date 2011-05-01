package gw.storage.javasvn.pathtree;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import gw.storage.StorageException;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import junit.framework.TestCase;

public class ProxyPathNodeTest extends TestCase
{
	private static final String USER="gwuser";
	private static final String PASSWORD="gwpassword";
	private static final String URL="svn://localhost:10000/branches";
	private SVNRepository repository;
	private long revision;
	
	public ProxyPathNodeTest()
	{
        DAVRepositoryFactory.setup();
        SVNRepositoryFactoryImpl.setup();
	}

	protected void setUp() throws Exception 
	{
		repository = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(URL));
		ISVNAuthenticationManager authManager =
            SVNWCUtil.createDefaultAuthenticationManager(USER, PASSWORD);
		repository.setAuthenticationManager(authManager);
		
		revision = repository.getLatestRevision();
	}

	public void testGetChild() throws StorageException, SVNException
	{
		RootNode root = new RootNode(repository, revision);
		
		// the most general case, get some stuff from the repository
		PathNode node = root.getChild("dirListing");
		assertEquals("/dirListing", node.getFullPath().toString());
		assertTrue(node.isDir());
		
		node = node.getChild("dir3");
		assertEquals("/dirListing/dir3", node.getFullPath().toString());
		assertTrue(node.isDir());
		
		// get a local node
		node.addChild(new PathNode("newNode", true));
		node = node.getChild("newNode");
		assertEquals("/dirListing/dir3/newNode", node.getFullPath().toString());
		assertTrue(node.isDir());
		
		// get nonexisting child
		node = root.getChild("nonexisting");
		assertTrue(node == null);
		
		// get a child from a file
		node = root.getChild("dirListing").getChild("f1").getChild("somechild");
		assertTrue(node == null);
		
		// now the offline checks
		root.getChild("dirListing").getChildren();
		repository.closeSession();
		repository.setAuthenticationManager(null);

		// get a node for the second time
		node = root.getChild("dirListing");
		assertEquals("/dirListing", node.getFullPath().toString());
		assertTrue(node.isDir());
		
		// get a child from a dir that has been checked out completely
		node = node.getChild("dir1");
		assertEquals("/dirListing/dir1", node.getFullPath().toString());
		assertTrue(node.isDir());
	}
	
	private Set<String> createPathMap(Collection<PathNode> nodes)
	{
		HashSet<String> set = new HashSet<String>();
		
		for ( PathNode node : nodes )
		{
			set.add(node.getFullPath().toString());
		}
		
		return set;
	}

	public void testGetChildren() throws StorageException, SVNException
	{
		RootNode root = new RootNode(repository, revision);
		Set<String> children;
		
		// everything from the repository, complete checkout
		PathNode node = root.getChild("dirListing");
		children = createPathMap(node.getChildren());
		assertTrue(children.contains("/dirListing/dir1"));
		assertTrue(children.contains("/dirListing/dir2"));
		assertTrue(children.contains("/dirListing/dir3"));
		assertTrue(children.contains("/dirListing/f1"));
		assertTrue(children.contains("/dirListing/f2"));
		assertEquals(5, children.size());
				
		// partly added earlier by proxy
		node = root.getChild("blame");
		node.getChild("somedir");
		children = createPathMap(node.getChildren());
		assertTrue(children.contains("/blame/blamefile"));
		assertTrue(children.contains("/blame/somedir"));
		assertEquals(2, children.size());
		
		// partly added earlier by hand
		node = root.getChild("getFile");
		node.addChild(new PathNode("someNewNode", true));
		children = createPathMap(node.getChildren());
		assertTrue(children.contains("/getFile/revision.txt"));
		assertTrue(children.contains("/getFile/test_string.txt"));
		assertTrue(children.contains("/getFile/someNewNode"));
		assertEquals(3, children.size());
		
		// get children of a file
		node = root.getChild("dirListing").getChild("f1");
		assertTrue(node.getChildren() == null);
		repository.setAuthenticationManager(null);
		
		// check offline
		repository.closeSession();
		node = root.getChild("dirListing");
		children = createPathMap(node.getChildren());
		assertTrue(children.contains("/dirListing/dir1"));
		assertTrue(children.contains("/dirListing/dir2"));
		assertTrue(children.contains("/dirListing/dir3"));
		assertTrue(children.contains("/dirListing/f1"));
		assertTrue(children.contains("/dirListing/f2"));
		assertEquals(5, children.size());
	}
}
