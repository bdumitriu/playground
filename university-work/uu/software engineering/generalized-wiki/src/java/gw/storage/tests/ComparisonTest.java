package gw.storage.tests;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.storage.StorageLogMessage;
import gw.storage.StorageStatusMessage;
import gw.storage.javasvn.SVNRAStorage;
import gw.storage.javasvn.virtualwc.VirtualWCStorage;
import gw.storage.javahl.SVNStorage;
import junit.framework.TestCase;

public class ComparisonTest extends TestCase
{

	protected void setUp() throws Exception
	{
		//_svnStorage = new SVNStorage(URL, USER, PASSWORD);
		_svnStorage = new SVNRAStorage(URL, USER, PASSWORD);
		_raStorage = new VirtualWCStorage(URL,USER,PASSWORD);
		//_raStorage = new SVNRAStorage(URL, USER, PASSWORD);
	}
	
	private Storage _svnStorage;
	private Storage _raStorage;
	private static final String URL="svn://127.0.0.1:10000/branches/";
//	private static final String URL="svn://localhost:9876";
	private static final String USER="gwuser";
	private static final String PASSWORD="gwpassword";
	private static final long REVISION=23;

//	it seems that the javahl implementation isn't working ...
//	so this test is disabled.
//	public void testBlame()
//	{	
//	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getFile(String)'
	 */
	public void testGetFileString() throws IOException, StorageException
	{
		InputStream svnInput;
		InputStream raInput;
		
		// compare an existing but uninteresting file
		svnInput = _svnStorage.getFile("blame/blamefile");
		raInput = _raStorage.getFile("blame/blamefile");
		compareInputStreams(svnInput, raInput);
		
		// compare a newly created file
		byte[] inputStuff = {'a', 'b', 'c'};
		OutputStream os = _svnStorage.storeFile("blame/newfile");
		os.write(inputStuff);
		os.close();
		os = _raStorage.storeFile("blame/newfile");
		os.write(inputStuff);
		os.close();
		
		svnInput = _svnStorage.getFile("blame/newfile");
		raInput = _raStorage.getFile("blame/newfile");
		compareInputStreams(svnInput, raInput);		
		
		
		// compare two nonexisting files
		try
		{
			svnInput = _svnStorage.getFile("somenonexisiting/file");
			fail("SVNStorage.getFile() did not throw an exception for non existing path!");
		}
		catch ( StorageException svnException )
		{
			try
			{
				raInput = _raStorage.getFile("somenonexisiting/file");
				fail("SVNRAStorage.getFile() did not throw an exception for non existing path!");
			}
			catch ( StorageException raException )
			{
				// error messages do not have to be equal
				//assertEquals(svnException, raException);
				assertTrue(true);
			}
		}
	}
	
	private void compareInputStreams(InputStream svnInput, InputStream raInput)
	{
		int svn = 1;
		int ra = 1;

		try
		{
			while ( svn != -1 && ra != -1 )
			{
				svn = svnInput.read();
				ra = raInput.read();
				
				assertEquals(svn, ra);
			}
		}
		catch ( IOException ioe )
		{
			fail(ioe.getMessage());
		}
		
		if ( svn != -1 || ra != -1 )
			fail("InputStream delivered by the javahl and javasvn implementations do not have the same length!");
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getFile(String, long)'
	 */
	public void testGetFileStringLong() throws StorageException, IOException
	{
		InputStream svnInput;
		InputStream raInput;
		
		// compare an existing but uninteresting file
		svnInput = _svnStorage.getFile("/blame/blamefile", 10);
		raInput = _raStorage.getFile("blame/blamefile", 10);
		compareInputStreams(svnInput, raInput);
	
		
		// compare two nonexisting files
		try
		{
			svnInput = _svnStorage.getFile("somenonexisiting/file", 5);
			fail("SVNStorage.getFile() did not throw an exception for non existing path!");
		}
		catch ( StorageException svnException )
		{
			try
			{
				raInput = _raStorage.getFile("somenonexisiting/file", 5);
				fail("SVNRAStorage.getFile() did not throw an exception for non existing path!");
			}
			catch ( StorageException raException )
			{
				// error messages do not have to be equal
				//assertEquals(svnException, raException);
				assertTrue(true);
			}
		}
		
		
		// compare a newly created file,
		// should even for the last revision not succeed
		byte[] inputStuff = {'a', 'b', 'c'};
		OutputStream os = _svnStorage.storeFile("blame/newfile");
		os.write(inputStuff);
		os.close();
		os = _raStorage.storeFile("blame/newfile");
		os.write(inputStuff);
		os.close();
		
		try
		{
			svnInput = _svnStorage.getFile("/blame/newfile", REVISION);
			fail("SVNStorage.getFile() did not throw an exception for non (yet) existing path!");
		}
		catch ( StorageException svnException )
		{
			try
			{
				raInput = _raStorage.getFile("blame/newfile/", REVISION);
				fail("SVNRAStorage.getFile() did not throw an exception for non (yet) existing path!");
			}
			catch ( StorageException raException )
			{
				assertTrue(true);
			}
		}	
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getDirListing(String)'
	 */
	public void testGetDirListingString() throws StorageException, IOException
	{
		Iterator<String> svnIt, raIt;
		
		// get dirlisting of existing dir in repository
		// NOTE: the javahl implementation requires us to start with a '/'
		svnIt = _svnStorage.getDirListing("/");
		raIt = _raStorage.getDirListing("/");
		compareDirListings(svnIt, raIt);
		
		
		// get dirlisting of nonexisting dir
		try
		{
			svnIt = _svnStorage.getDirListing("/somenonexisting/path");
			fail("SVNStorage.getDirListing() did not throw an exception for nonexisting path!");
		}
		catch ( StorageException svnException )
		{
			try
			{
				raIt = _raStorage.getDirListing("/somenonexisting/path");
				fail("SVNRAStorage.getDirListing() did not throw an exception for nonexisting path!");
			}
			catch ( StorageException raException )
			{
				// error messages do not have to be equal
				//assertEquals(svnException, raException);
				assertTrue(true);
			}
		}
		
		// get dirlisting of a file instead of a directory
		svnIt = _svnStorage.getDirListing("/blame/blamefile");
		raIt = _raStorage.getDirListing("blame/blamefile");
		compareDirListings(svnIt, raIt);
		
		// get dirlisting of a added dir (not yet committed)
		createDirWithContents(_svnStorage);
		createDirWithContents(_raStorage);
		svnIt = _svnStorage.getDirListing("/dirListing/dir1");
		raIt = _raStorage.getDirListing("/dirListing/dir1");
		compareDirListings(svnIt, raIt);
		
		// get dirlisting of an empty dir
		svnIt = _svnStorage.getDirListing("/dirListing/dir3/dir31/dir311/dir3111/dir31111");
		raIt = _raStorage.getDirListing("/dirListing/dir3/dir31/dir311/dir3111/dir31111/");
		compareDirListings(svnIt, raIt);
	}
	
	private void createDirWithContents(Storage storage) throws StorageException, IOException
	{
    	storage.makeDirectory("/dirListing/dir1/dir13");      
        storage.storeFile("/dirListing/dir1/f12").close();
        storage.makeDirectory("/dirListing/dir1/dir13/dir131");
        storage.storeFile("/dirListing/dir1/dir13/f131").close();
	}
	
	private void compareDirListings(Iterator svnIt, Iterator raIt)
	{
		HashMap svnMap = new HashMap();
		HashMap raMap = new HashMap();

		while ( svnIt.hasNext() )
			svnMap.put(svnIt.next(), null);
		while ( raIt.hasNext() )
			raMap.put(raIt.next(), null);
		
		assertEquals(svnMap, raMap);			
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getDirListing(String, boolean)'
	 */
	public void testGetDirListingStringBoolean() throws StorageException, IOException
	{
		Iterator<String> svnIt, raIt;
		
		// get dirlisting of existing dir in repository
		// NOTE: the javahl implementation requires us to start with a '/'
		svnIt = _svnStorage.getDirListing("/", true);
		raIt = _raStorage.getDirListing("/", true);
		compareDirListings(svnIt, raIt);
		
		// get dirlisting of a file instead of a directory
		svnIt = _svnStorage.getDirListing("/blame/blamefile", true);
		raIt = _raStorage.getDirListing("blame/blamefile", true);
		compareDirListings(svnIt, raIt);
		
		// get dirlisting of a added dir (not yet committed)
		createDirWithContents(_svnStorage);
		createDirWithContents(_raStorage);
		svnIt = _svnStorage.getDirListing("/", true);
		raIt = _raStorage.getDirListing("/", true);
		compareDirListings(svnIt, raIt);
		
		// get dirlisting of an empty dir
		svnIt = _svnStorage.getDirListing("/blame/somedir", true);
		raIt = _raStorage.getDirListing("/blame/somedir", true);
		compareDirListings(svnIt, raIt);
	}

// Test disabled because javahl has inconsistent output that should not be used
// as a reference
//	public void testGetDirListingStringLongBoolean()
//	{
//	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getStatus(String)'
	 */
	public void testGetStatusString() throws IOException, StorageException
	{
		Map<String, StorageStatusMessage> svnMap, raMap;

		// get dirlisting of existing dir in repository
		svnMap = _svnStorage.getStatus("/status");
		raMap = _raStorage.getStatus("status");
		assertEquals(svnMap, raMap);
		
		// get status of nonexisting dir
		try
		{
			svnMap = _svnStorage.getStatus("/somenonexisting/path");
			fail("SVNStorage.getStatus() did not throw an exception for nonexisting path!");
		}
		catch ( StorageException svnException )
		{
			try
			{
				raMap = _raStorage.getStatus("/somenonexisting/path");
				fail("SVNRAStorage.getStatus() did not throw an exception for nonexisting path!");
			}
			catch ( StorageException raException )
			{
				// error messages do not have to be equal
				//assertEquals(svnException, raException);
				assertTrue(true);
			}
		}
		
		// get status of a file
		svnMap = _svnStorage.getStatus("/blame/blamefile");
		raMap = _raStorage.getStatus("blame/blamefile");
		assertEquals(svnMap, raMap);
		
		// get status of a added dir (not yet committed)
		createDirWithContents(_svnStorage);
		createDirWithContents(_raStorage);
		svnMap = _svnStorage.getStatus("/dirListing/dir1/dir13");
		raMap = _raStorage.getStatus("/dirListing/dir1/dir13");
		assertEquals(svnMap, raMap);
		
		// get status of an empty dir
		svnMap = _svnStorage.getStatus("/blame/somedir");
		raMap = _raStorage.getStatus("/blame/somedir");
		assertEquals(svnMap, raMap);
	}
	
//	private void removeSomeStuff(Storage storage) throws StorageException
//	{
//		storage.deleteFile("/dirListing/dir2", false);
//		storage.deleteFile("/dirListing/f1", false);
//		storage.deleteFile("/dirListing/dir3/dir31/f311", false);
//	}
	
	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getStatus(String, boolean)'
	 */
	public void testGetStatusStringBoolean() throws StorageException, IOException
	{
		Map<String, StorageStatusMessage> svnMap, raMap;

		// get dirlisting of existing dir in repository
		svnMap = _svnStorage.getStatus("/status", true);
		raMap = _raStorage.getStatus("status", true);
		assertEquals(svnMap, raMap);
		
		// get status of a file
		svnMap = _svnStorage.getStatus("/blame/blamefile", true);
		raMap = _raStorage.getStatus("blame/blamefile", true);
		assertEquals(svnMap, raMap);
		
		// get status of a added dir (not yet committed)
		createDirWithContents(_svnStorage);
		createDirWithContents(_raStorage);
		svnMap = _svnStorage.getStatus("/dirListing/dir1/dir13", true);
		raMap = _raStorage.getStatus("/dirListing/dir1/dir13", true);
		assertEquals(svnMap, raMap);
		
		// get status of an empty dir
		svnMap = _svnStorage.getStatus("/blame/somedir", true);
		raMap = _raStorage.getStatus("/blame/somedir", true);
		assertEquals(svnMap, raMap);
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.fileExists(String)'
	 */
	public void testFileExists() throws IOException, StorageException
	{
		// a file in the repo
		assertTrue(_svnStorage.fileExists("status/somefile"));
		assertEquals(
				_svnStorage.fileExists("status/somefile"),
				_raStorage.fileExists("status/somefile"));
		
		// a nonexisting file
		assertFalse(_svnStorage.fileExists("nonexistingfile"));
		assertEquals(
				_svnStorage.fileExists("nonexistingfile"),
				_raStorage.fileExists("nonexistingfile"));
		
		// a file that's just been added
		createDirWithContents(_svnStorage);
		createDirWithContents(_raStorage);
		assertFalse(_svnStorage.isDirectory("/dirListing/dir1/f12"));
		assertEquals(
				_svnStorage.isDirectory("/dirListing/dir1/f12"),
				_raStorage.isDirectory("/dirListing/dir1/f12"));
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.isDirectory(String)'
	 */
	public void testIsDirectory() throws StorageException, IOException
	{
		// a dir in the repo
		assertTrue(_svnStorage.isDirectory("/status"));
		assertEquals(
				_svnStorage.isDirectory("/status"),
				_raStorage.isDirectory("/status"));
		
		// a file in the repo
		assertFalse(_svnStorage.isDirectory("status/somefile"));
		assertEquals(
				_svnStorage.isDirectory("status/somefile"),
				_raStorage.isDirectory("status/somefile"));
		
		// a nonexisting file
		assertFalse(_svnStorage.isDirectory("nonexistingfile"));
		assertEquals(
				_svnStorage.isDirectory("nonexistingfile"),
				_raStorage.isDirectory("nonexistingfile"));
		
		// a directory that's just been added
		createDirWithContents(_svnStorage);
		createDirWithContents(_raStorage);
		assertTrue(_svnStorage.isDirectory("/dirListing/dir1/dir13"));
		assertEquals(
				_svnStorage.isDirectory("/dirListing/dir1/dir13"),
				_raStorage.isDirectory("/dirListing/dir1/dir13"));
		
		// a file that's just been added
		assertFalse(_svnStorage.isDirectory("/dirListing/dir1/f12"));
		assertEquals(
				_svnStorage.isDirectory("/dirListing/dir1/f12"),
				_raStorage.isDirectory("/dirListing/dir1/f12"));
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getLog(String)'
	 */
	public void testGetLog() throws StorageException, IOException 
	{
		SortedSet<StorageLogMessage> svnSet, raSet;
		
		// get log of the root
		svnSet = _svnStorage.getLog("/");
		raSet = _raStorage.getLog("/");
		compareLogs(svnSet, raSet);
				
		// get log of some other dir
		svnSet = _svnStorage.getLog("/update");
		raSet = _raStorage.getLog("update");
		compareLogs(svnSet, raSet);
		
		// get log of a nonexisting dir
		try
		{
			svnSet = _svnStorage.getLog("/somenonexisting/path");
			fail("SVNStorage.getLog() did not throw an exception for nonexisting path!");
		}
		catch ( StorageException svnException )
		{
			try
			{
				raSet = _raStorage.getLog("/somenonexisting/path");
				fail("SVNRAStorage.getLog() did not throw an exception for nonexisting path!");
			}
			catch ( StorageException raException )
			{
				// error messages do not have to be equal
				//assertEquals(svnException, raException);
				assertTrue(true);
			}
		}
		
		// get log of a dir where stuff has changed
		createDirWithContents(_svnStorage);
		createDirWithContents(_raStorage);
		svnSet = _svnStorage.getLog("/dirListing");
		raSet = _raStorage.getLog("dirListing");
		compareLogs(svnSet, raSet);
	}
	
	private void compareLogs(SortedSet svnSet, SortedSet raSet)
	{
		Object[] svnArray, raArray;
		
		svnArray = svnSet.toArray();
		raArray = raSet.toArray();
		
		assertEquals(svnArray.length, raArray.length);
		
		for ( int i = 0; i < svnArray.length; i++ )
			assertEquals(svnArray[i], raArray[i]);
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getProperties(String)'
	 */
	public void testGetProperties() throws StorageException
	{
		Map svnMap, raMap;
		
		// TODO: finish this one
		
		// get properties of a file in repo
		svnMap = _svnStorage.getProperties("/blame/blamefile");
		raMap = _raStorage.getProperties("/blame/blamefile");
		assertEquals(svnMap, raMap);
		
		// get properties of a dir in the repo
		svnMap = _svnStorage.getProperties("/dirListing");
		raMap = _raStorage.getProperties("dirListing");
		assertEquals(svnMap, raMap);
		
		// get properties of a file with changed properties
		_svnStorage.setProperty("/blame/blamefile","testproperty", "somevalue", false);
		_raStorage.setProperty("/blame/blamefile","testproperty", "somevalue", false);
		svnMap = _svnStorage.getProperties("/blame/blamefile");
		raMap = _raStorage.getProperties("/blame/blamefile");
		assertEquals(svnMap, raMap);
		
		// get properties of a non existing path
		try
		{
			svnMap = _svnStorage.getProperties("/somenonexisting/path");
			fail("SVNStorage.getProperties() did not throw an exception for nonexisting path!");
		}
		catch ( StorageException svnException )
		{
			try
			{
				raMap = _raStorage.getProperties("/somenonexisting/path");
				fail("SVNRAStorage.getProperties() did not throw an exception for nonexisting path!");
			}
			catch ( StorageException raException )
			{
				// error messages do not have to be equal
				//assertEquals(svnException, raException);
				assertTrue(true);
			}
		}
	}

	/*
	 * Test method for 'gw.storage.javahl.SVNStorage.getConflicts()'
	 */
	public void testGetConflicts() throws StorageException, IOException
	{
		Iterator<String> svnIt, raIt;
		
		// get conflicts if we know there are no changes at all
		svnIt = _svnStorage.getConflicts();
		raIt = _raStorage.getConflicts();
		compareDirListings(svnIt, raIt);
		
		// get conflicts if there are changes and conflicts
		Storage conflictor = new SVNStorage(URL, USER, PASSWORD);
		conflictor.setProperty("/blame/blamefile","testproperty", "someothervalue", false);
		conflictor.commit("trying to create a conflict");
		_svnStorage.setProperty("/blame/blamefile","testproperty", "somevalue", false);
		_raStorage.setProperty("/blame/blamefile","testproperty", "somevalue", false);
		_svnStorage.update();
		_raStorage.update();
		
		svnIt = _svnStorage.getConflicts();
		raIt = _raStorage.getConflicts();
		compareDirListings(svnIt, raIt);
	}
}
