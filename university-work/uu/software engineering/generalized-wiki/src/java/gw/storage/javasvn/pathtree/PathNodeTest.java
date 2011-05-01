package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.NotADirException;
import gw.storage.javasvn.virtualwc.CheckoutManager;

import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import junit.framework.TestCase;

public class PathNodeTest extends TestCase
{
	private RootNode root;
	
	private static final String USER="gwuser";
	private static final String PASSWORD="gwpassword";
	private static final String URL="svn://localhost:10000/branches/dirListing/";
	
	public PathNodeTest()
	{
        DAVRepositoryFactory.setup();
        SVNRepositoryFactoryImpl.setup();
	}

	@Override
	protected void setUp() throws Exception 
	{
		//FIXME: disabled this temporarily because now i really have to use mockobjects
		// TODO: setting up this way is slow and, what is worse,
		// introduces more dependencies for this test, so the node
		// structure should be created manually
//		SVNRepository repository = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(URL));
//		ISVNAuthenticationManager authManager =
//            SVNWCUtil.createDefaultAuthenticationManager(USER, PASSWORD);
//		repository.setAuthenticationManager(authManager);
//		
//		long revision = repository.getLatestRevision();
//		
//		root = new RootNode();
//		CheckoutManager cm = new CheckoutManager(repository, root);
//		cm.checkout(new Path("/"), true, revision);
	}

	public void testgetFullPath() throws NodeNotFoundException, StorageException
	{
		// first easy, the root itself
		assertEquals("/", root.getFullPath().toString());
		
		// now some deep subdirectory
		String path = "/dir1/dir12/dir122/f1221";
		PathNode node = root.getNode(new Path(path));
		
		assertEquals(path, node.getFullPath().toString());
	}

	public void testRemoveChild() throws NotADirException, NodeNotFoundException, StorageException
	{
		// remove a nonexisting child
		root.removeChild("someNonExistingChild");
		
		// remove a child of the root
		PathNode child = root.getChild("dir2");
		root.removeChild("dir2");
		assertTrue(child.getParent() == null);
		assertTrue(root.getChild("dir2") == null);
		
		// remove a child of a file
		PathNode node = root.getChild("dir1").getChild("dir12").getChild("dir122").getChild("f1221");
		node.removeChild("blah");
	}

	public void testGetNode() throws NodeNotFoundException, StorageException
	{
		// get the root
		PathNode node = root.getNode(new Path("/"));
		assertEquals("/", node.getFullPath().toString());
		
		// get some other file
		String filename = "/dir3/dir31/f312";
		node = root.getNode(new Path(filename));
		assertEquals(filename, node.getFullPath().toString());
		
		// get a nonexisting node which ends in a directory
		try
		{
			filename = "/dir3/somenonexistingdir";
			node = root.getNode(new Path(filename));
			fail("Getting a nonexisting node did not throw an exception!");
		}
		catch ( NodeNotFoundException e )
		{
			assertEquals(e.getFullPath().toString().toString(), filename);
			assertEquals(e.getLastNode().getFullPath().toString(), "/dir3");
			assertEquals(e.getRemainingPath().toString(), "/somenonexistingdir");
		}
		
		// get a nonexisting node which ends in a file
		// get a nonexisting node which ends in a directory
		try
		{
			filename = "/f2/nofile";
			node = root.getNode(new Path(filename));
			fail("Getting a nonexisting node did not throw an exception!");
		}
		catch ( NodeNotFoundException e )
		{
			assertEquals(e.getFullPath().toString(), filename);
			assertEquals(e.getLastNode().getFullPath().toString(), "/f2");
			assertEquals(e.getRemainingPath().toString(), "/nofile");
		}
		
		// TODO: get a path which has been removed
	}
	
	public void testGetChild() throws NotADirException, StorageException
	{
		// get a nonexisting child
		assertTrue(root.getChild("someNonExistingChild") == null);
		
		// get an existing child
		assertEquals("/dir1", root.getChild("dir1").getFullPath().toString());
		
		// get a child from a dir
		assertTrue(root.getChild("f1").getChild("blah") == null);
	}
}
