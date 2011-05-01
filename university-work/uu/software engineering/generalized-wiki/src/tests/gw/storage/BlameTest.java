package gw.storage;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Date;
import java.util.Iterator;
import java.util.Random;

import gw.storage.StorageBlameLine;
import gw.storage.StorageException;
import gw.storage.javasvn.SVNRAStorage;
import junit.framework.TestCase;

public class BlameTest extends TestCase
{
	private static final String USER="gwuser";
	private static final String PASSWORD="gwpassword";
	private static final String URL="svn://localhost:10000/branches/blame/";
	private SVNRAStorage _storage;
	
	protected void setUp() throws Exception
	{
		_storage = new SVNRAStorage(URL,USER,PASSWORD);
	}

	public void testBlameValidFromRepository() throws StorageException
	{
		// just get the blame of a valid file from first to the last revision
		Iterator<StorageBlameLine> it = _storage.blame("blamefile", 0, 12);
		assertEquals(new StorageBlameLine(new Date(1129237435627l), 11, "gwuser", "third change"), it.next());
		assertEquals(new StorageBlameLine(new Date(1129237353902l), 9, "gwuser", "initial version"), it.next());
		assertEquals(new StorageBlameLine(new Date(1129237467060l), 12, "gwuser", "second revision now edited again"), it.next());
		assertFalse(it.hasNext());
		
		// now get the blame of only part of the revisions
		it = _storage.blame("blamefile", 5, 10);
		assertEquals(new StorageBlameLine(new Date(1129237353902l), 9, "gwuser", "initial version"), it.next());
		assertEquals(new StorageBlameLine(new Date(1129237418924l), 10, "gwuser", "second revision"), it.next());
		assertFalse(it.hasNext());
		
		// now get the blame of the revision starting before it was created
		it = _storage.blame("blamefile", 10, 12);
		assertEquals(new StorageBlameLine(new Date(1129237435627l), 11, "gwuser", "third change"), it.next());
		assertEquals(new StorageBlameLine(new Date(1129237418924l), 10, "gwuser", "initial version"), it.next());
		assertEquals(new StorageBlameLine(new Date(1129237467060l), 12, "gwuser", "second revision now edited again"), it.next());
		assertFalse(it.hasNext());
	}
	
	public void testBlameInvalidNewPath() throws StorageException, IOException
	{
		OutputStream out = _storage.storeFile("newfile");
		Random rand = new Random();
		
		for ( int i = 0; i < 25; i++ )
			out.write(rand.nextInt());
		
		out.close();
		
		try
		{
			_storage.blame("newfile", 10, _storage.getLatestRevision());
			fail("Blame did not throw any exception for a path that does not (yet) exist on the repository!");
		}
		catch ( StorageException e )
		{
			assertTrue(true);
		}
	}
	
	public void testBlameInvalidRevisionRange()
	{
		// test a revision range that's negative
		try
		{
			@SuppressWarnings("unused") Iterator<StorageBlameLine> it =
				_storage.blame("blamefile", 10, 1);
			fail("No exception thrown!");
		}
		catch ( StorageException e )
		{
			// success
		}
	}
	
	public void testBlameInvalidNonexistingPath()
	{
		// test a non exisiting path
		try
		{
			@SuppressWarnings("unused") Iterator<StorageBlameLine> it =
				_storage.blame("nonexisting", 0, 12);
			fail("No exception thrown!");
		}
		catch ( StorageException e )
		{
			// success
		}
	}
	
	public void testBlameInvalidDirectory()
	{
		// try to get the blame of a directory
		try
		{
			@SuppressWarnings("unused") Iterator<StorageBlameLine> it =
				_storage.blame("somedir", 0, 12);
			fail("No exception thrown!");
		}
		catch ( StorageException e )
		{
			// success
		}
	}
}
