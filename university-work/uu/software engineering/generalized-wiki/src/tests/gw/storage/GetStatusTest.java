package gw.storage;

import java.io.IOException;
import java.util.Date;
import java.util.Map;

import gw.storage.StorageException;
import gw.storage.StorageStatusMessage;
import gw.storage.javasvn.SVNRAStorage;
import junit.framework.TestCase;

public class GetStatusTest extends TestCase
{
	private SVNRAStorage _storage;
	private static final String URL = "svn://localhost:10000/";
	private static final String USER = "gwuser";
	private static final String PASSWORD = "gwpassword";
	
	@Override
	protected void setUp() throws Exception
	{
		_storage = new SVNRAStorage(URL, USER, PASSWORD);
	}

	/*
	 * Test method for 'gw.storage.javasvn.SVNRAStorage.getStatus(String, boolean)'
	 */
	public void testGetStatusValidNonrecursive() throws StorageException
	{
		// get a whole directory
		Map<String, StorageStatusMessage> result =
			_storage.getStatus("/branches/status", false);
		
		//System.out.println(result);

		assertTrue(
				result.containsKey("/branches/status/somefile")
				&& result.containsKey("/branches/status/somedir")
				&& result.containsKey("/branches/status")
				&& result.containsKey("/branches/status/someotherfile"));
		assertEquals(4, result.size());
		
		// get a single file
		result = _storage.getStatus("/branches/status/somefile", false);
		assertTrue(result.containsKey("/branches/status/somefile"));
		assertEquals(1, result.size());
	}
	

	
	/*
	 * Test method for 'gw.storage.javasvn.SVNRAStorage.getStatus(String, boolean)'
	 */
	public void testGetStatusValidRecursive() throws StorageException
	{
		// try to get an existing files in path
		Map<String, StorageStatusMessage> result =
			_storage.getStatus("/branches/status", true);
		
		assertTrue(
				result.containsKey("/branches/status/somefile")
				&& result.containsKey("/branches/status/somedir")
				&& result.containsKey("/branches/status")
				&& result.containsKey("/branches/status/someotherfile")
				&& result.containsKey("/branches/status/somedir/een")
				&& result.containsKey("/branches/status/somedir/twee")
				&& result.containsKey("/branches/status/somedir/drie")
				&& result.containsKey("/branches/status/somedir/vier"));
		assertEquals(8, result.size());

		TestStorageStatusMessage reference = new TestStorageStatusMessage(
				new Date(1128945921462l), 2, "gwuser", true,
				TestStorageStatusMessage.StatusType.NONE,
				_storage.getLatestRevision());
		
		assertEquals(reference, result.get("/branches/status/somefile"));
		assertEquals(reference,	result.get("/branches/status/somedir/drie"));
		
		reference = new TestStorageStatusMessage(
				new Date(1128945921462l), 2, "gwuser", false,
				TestStorageStatusMessage.StatusType.NONE, 
				_storage.getLatestRevision());
		
		assertEquals(reference, result.get("/branches/status/somedir"));
	}
	
	public void testGetStatusValidRecursiveEdited() throws StorageException, IOException
	{
		// try to get a path with moved, added and removed files
		_storage.makeDirectory("branches/status/newDir");
		_storage.storeFile("branches/status/newDir/vijf").close();
		_storage.storeFile("branches/status/newDir/zes").close();
		_storage.deleteFile("branches/status/somedir", false);
		//_storage.copyFile("branches/status/somefile", "branches/status/somecopy");
		_storage.deleteFile("branches/status/someotherfile", false);
		
		Map<String, StorageStatusMessage> result =
			_storage.getStatus("branches/status", true);
		
		assertTrue(
				result.containsKey("/branches/status/somefile")
				&& result.containsKey("/branches/status")
				&& result.containsKey("/branches/status/newDir/zes")
				&& result.containsKey("/branches/status/newDir")
				&& result.containsKey("/branches/status/newDir/vijf")
				);
		assertEquals(5, result.size());
		
		TestStorageStatusMessage reference = new TestStorageStatusMessage(null, -1, null, true,
				TestStorageStatusMessage.StatusType.ADDED, 0);
		
		assertEquals(reference, result.get("/branches/status/newDir/zes"));
	}
	
	public void testGetFileRecursively() throws StorageException
	{
		// we now allow to get a file's (as opposed to a directory)
		// status to be 'retrieved' with the recursive boolean
		// set to true
		
		Map<String, StorageStatusMessage> result =
			_storage.getStatus("branches/status/somefile", true);
		
		assertTrue("Too many or too few status messages returned!", result.size() == 1);
		assertTrue("Actual (correct) status message is missing!", result.containsKey("/branches/status/somefile"));
	}
	
	/*
	 * Test method for 'gw.storage.javasvn.SVNRAStorage.getStatus(String, boolean)'
	 */
	public void testGetStatusInvalid()
	{
		try
		{
			// nonexisting path
			@SuppressWarnings("unused") Map<String, StorageStatusMessage> result =
				_storage.getStatus("branches/status/nonexistingpath", true);
			
			fail("Status of nonexisting path did not throw any exception.");
		}
		catch ( StorageException e )
		{
			assertTrue(true);
		}
	}
}
