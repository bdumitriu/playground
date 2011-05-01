package gw.storage.javasvn.virtualwc;


import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;

import java.io.IOException;


import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.NotAFileException;
import gw.storage.javasvn.exceptions.NotADirException;
import gw.storage.javasvn.pathtree.MockRootNode;
import gw.storage.javasvn.pathtree.PathNode;
import gw.storage.javasvn.pathtree.RootNode;
import junit.framework.TestCase;

public class AddManagerTest extends TestCase
{

	public void testMakeDirectoryInSteps() throws NotADirException, StorageException
	{
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		am.makeDirectory(new Path("d1"));
		am.makeDirectory(new Path("d2"));
		am.makeDirectory(new Path("d1/d12"));
		
		checkDirNode(root.getChild("d1"));
		checkDirNode(root.getChild("d1").getChild("d12"));
		checkDirNode(root.getChild("d2"));
	}
	
	public void testMakeDirectoryExistingDir() throws StorageException, NotADirException
	{
		// test for a dir that already exists
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		am.makeDirectory(new Path("d1/d2/d3"));
		
		try
		{
			am.makeDirectory(new Path("d1/d2"));
			fail("Creating an already existing dir didn't throw an exception!");
		}
		catch ( StorageException e )
		{
			assertTrue(true);
		}
	}
	
	public void testMakeDirectoryInFile() throws Exception
	{
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		File tmpFile = File.createTempFile("addManagerTest", ".txt");
		am.storeFile(new Path("/somedir/anotherdir/newfile"),tmpFile);
	
		// test for a path that exists as a file
		try
		{
			am.makeDirectory(new Path("/somedir/anotherdir/newfile"));
			fail("Creating a dir over an existing file did not throw an exception!");
		}
		catch ( NotADirException e )
		{
			assertTrue(true);
		}
		
		// test for a path that has a file as parent
		try
		{
			am.makeDirectory(new Path("/somedir/anotherdir/newfile/otherdir"));
			fail("Creating a dir in an existing file did not throw an exception!");
		}
		catch ( NotADirException e )
		{
			assertTrue(true);
		}
	}
	
	public void testMakeDeepDirectory() throws StorageException, NotADirException
	{
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		am.makeDirectory(new Path("d1/d11/d111/d1111"));
		am.makeDirectory(new Path("d1/d12/d121"));
		
		PathNode checknode = root.getChild("d1");
		checkDirNode(checknode);
		
		checknode = checknode.getChild("d11");
		checkDirNode(checknode);
		
		checknode = checknode.getChild("d111");
		checkDirNode(checknode);
		
		checknode = checknode.getChild("d1111");
		checkDirNode(checknode);
		
		checknode = root.getChild("d1");
		checkDirNode(checknode);
		
		checknode = checknode.getChild("d12");
		checkDirNode(checknode);

	}
	
	private void checkDirNode(PathNode node) throws NotADirException, StorageException
	{
		assertTrue(node.isDir());
		assertTrue(node.getData().isAdd());
		assertTrue(node.getParent().getChild(node.getPath()) == node);
	}

	public void testStoreNewFileSimple() throws IOException, NotADirException, NotAFileException, StorageException
	{
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		// create a new file
		File tmpFile = File.createTempFile("addManagerTest", ".txt");
		fillFile(tmpFile, FILEDATA1);
		am.storeFile(new Path("newfile"),tmpFile);
		
		PathNode node = root.getChild("newfile");
		assertTrue(node.getData().isAdd());
		assertTrue(node.hasChanges());
		checkInputStream(node.getData().getFile(), FILEDATA1);
		assertTrue(node.isFile());
		
		// modify an existing, but not committed file
		tmpFile = File.createTempFile("addManagerTest", ".txt");
		fillFile(tmpFile, FILEDATA2);
		am.storeFile(new Path("newfile"),tmpFile);
		
		checkInputStream(node.getData().getFile(), FILEDATA2);
		assertTrue(node.getData().isAdd());
		assertTrue(node.isFile());
		
	}
	
	private static final byte[] FILEDATA1 = {2, 3, 4, 5};
	private static final byte[] FILEDATA2 = {2, 3, 4, 5};
	
	private void fillFile(File file, byte[] data) throws IOException
	{
        BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(file));
        out.write(data);
        out.close();
	}
	
	private void checkInputStream(InputStream is, byte[] originalData) throws IOException
	{
		byte[] data = new byte[originalData.length];
		assertEquals(originalData.length, is.read(data));
		
		for ( int i = 0; i < data.length; i++ )
			assertEquals(originalData[i], data[i]);
	}
	
	public void testStoreNewFileDeep() throws Exception
	{
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		// create a new file
		File tmpFile = File.createTempFile("addManagerTest", ".txt");
		fillFile(tmpFile, FILEDATA1);
		am.storeFile(new Path("/somedir/anotherdir/newfile"),tmpFile);
		
		PathNode node = root.getNode(new Path("/somedir/anotherdir/newfile"));
		checkInputStream(node.getData().getFile(), FILEDATA1);
		assertTrue(node.getData().isAdd());
		assertTrue(node.isFile());
		
		// modify an existing, but not committed file
		tmpFile = File.createTempFile("addManagerTest", ".txt");
		fillFile(tmpFile, FILEDATA1);
		am.storeFile(new Path("/somedir/somedir/newfile"),tmpFile);
		tmpFile = File.createTempFile("addManagerTest", ".txt");
		fillFile(tmpFile, FILEDATA2);
		am.storeFile(new Path("/somedir/somedir/newfile"),tmpFile);
		
		node = root.getNode(new Path("/somedir/somedir/newfile"));
		checkInputStream(node.getData().getFile(), FILEDATA2);
		assertTrue(node.getData().isAdd());
		assertTrue(node.isFile());
	}
	
	public void testStoreModFile() throws Exception
	{
		RootNode root = new MockRootNode();
		PathNode dirNode = new PathNode("somedir", true);
		root.addChild(dirNode);
		PathNode fileNode = new PathNode("somefile", false);
		dirNode.addChild(fileNode);
		fileNode.setData(new MockNodeData());
		
		AddManager am = new AddManager(root);
		
		// modify a file
		File tmpFile = File.createTempFile("addManagerTest", ".txt");
		fillFile(tmpFile, FILEDATA1);
		am.storeFile(new Path("/somedir/somefile"),tmpFile);
		
		PathNode node = root.getNode(new Path("/somedir/somefile"));
		checkInputStream(node.getData().getFile(), FILEDATA1);
		assertTrue(node.getData().isModFile());
		assertTrue(node.isFile());
	}
	
	public void testStoreFileInFile() throws Exception
	{
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		File tmpFile = File.createTempFile("addManagerTest", ".txt");
		am.storeFile(new Path("/somedir/anotherdir/newfile"),tmpFile);
		
		// create a new file in another file
		try
		{
			am.storeFile(new Path("/somedir/anotherdir/newfile/newfile"), tmpFile);
			fail("Creating a file within another file did not throw an exception!");
		}
		catch ( NotADirException e )
		{
			assertTrue(true);
		}
	}
	
	public void testModifyFileDirectory() throws Exception
	{
		RootNode root = new MockRootNode();
		AddManager am = new AddManager(root);
		
		File tmpFile = File.createTempFile("addManagerTest", ".txt");
		am.storeFile(new Path("/somedir/anotherdir/newfile"),tmpFile);
		
		// try to modify a directory as if it were a file
		try
		{
			am.storeFile(new Path("/somedir/anotherdir"), tmpFile);
			fail("Editing a directory did not throw an exception!");
		}
		catch ( NotAFileException e )
		{
			assertTrue(true);
		}
	}
	
}
