package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.NotAFileException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import junit.framework.TestCase;

public class ProxyNodeDataTest extends TestCase
{
	private static final String USER="gwuser";
	private static final String PASSWORD="gwpassword";
	private static final String URL="svn://localhost:10000";
	private SVNRepository repository;
	private long revision;
	
	public ProxyNodeDataTest()
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
	
    private void compareFileContents(String reference, InputStream in) throws IOException
    {
    	StringBuilder builder = new StringBuilder();
    	BufferedReader reader = new BufferedReader(
    			new InputStreamReader(in));
    	
    	String line;
    	
    	while ( (line = reader.readLine()) != null )
    		builder.append(line);
    	
    	assertEquals(reference, builder.toString());
    }

	public void testGetFile() throws NodeNotFoundException, StorageException, IOException
	{
		RootNode root = new RootNode(repository, revision);
		
		// get a file and make sure it's contents are ok
		PathNode node = root.getNode(new Path("/branches/getFile/test_string.txt"));
		compareFileContents("test string", node.getData().getFile());
				
		// now get it again but first sabotage the repository
		repository.setAuthenticationManager(null);
		compareFileContents("test string", node.getData().getFile());	

		// try to get a file from a dir
		try
		{
			node = root.getNode(new Path("/branches/getFile"));
			node.getData().getFile();
			fail("Getting the file of a directory didn't throw an exception!");
		}
		catch ( NotAFileException e )
		{
			assertTrue(true);
		}
	}

	public void testGetProperties() throws Exception
	{
		RootNode root = new RootNode(repository, revision);
		
		// get a file and make sure it's contents are ok
		PathNode node = root.getNode(new Path("/branches/getFile/test_string.txt"));
		Map<String, String> result = node.getData().getProperties();
		Map<String, String> reference = new HashMap<String, String>();
		reference.put("svn:entry:checksum", "f299060e0383392ebeac64b714eca7e3");
		reference.put("svn:entry:revision", "28");
		reference.put("svn:entry:last-author", "gwuser");
		reference.put("svn:entry:uuid", "624ea1e4-a002-0410-86e2-ab83d833d84b");
		reference.put("svn:entry:committed-date", "2005-10-25T22:33:19.293707Z");
		reference.put("svn:entry:committed-rev", "17");
		assertEquals(reference, result);
		
		// get the root's properties
		node = root;
		result = node.getData().getProperties();
		reference = new HashMap<String, String>();
		reference.put("svn:entry:last-author", "gwuser");
		reference.put("svn:entry:uuid", "624ea1e4-a002-0410-86e2-ab83d833d84b");
		reference.put("svn:entry:committed-date", "2005-11-02T00:43:26.962278Z");
		reference.put("svn:entry:committed-rev", "28");
		assertEquals(reference, result);
		
		// get some dir's properties
		node = root.getNode(new Path("/branches/getFile"));
		result = node.getData().getProperties();
		reference = new HashMap<String, String>();
		reference.put("svn:entry:last-author", "gwuser");
		reference.put("svn:entry:uuid", "624ea1e4-a002-0410-86e2-ab83d833d84b");
		reference.put("svn:entry:committed-date", "2005-10-25T22:52:40.649594Z");
		reference.put("svn:entry:committed-rev", "19");
		assertEquals(reference, result);
				
		// now get it again but first sabotage the repository
		repository.setAuthenticationManager(null);
		node = root.getNode(new Path("/branches/getFile/test_string.txt"));
		result = node.getData().getProperties();
		reference = new HashMap<String, String>();
		reference.put("svn:entry:checksum", "f299060e0383392ebeac64b714eca7e3");
		reference.put("svn:entry:revision", "28");
		reference.put("svn:entry:last-author", "gwuser");
		reference.put("svn:entry:uuid", "624ea1e4-a002-0410-86e2-ab83d833d84b");
		reference.put("svn:entry:committed-date", "2005-10-25T22:33:19.293707Z");
		reference.put("svn:entry:committed-rev", "17");
		assertEquals(reference, result);
		
	}

}
