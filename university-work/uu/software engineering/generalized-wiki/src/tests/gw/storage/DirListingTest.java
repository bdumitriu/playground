package gw.storage;

import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import gw.storage.StorageDirEntry;
import gw.storage.javasvn.SVNRAStorage;
import junit.framework.*;

/**
 * Test cases for checking the getDirListing methods of the gw.storage.javasvn.SVNRAStorage.
 *
 * @author Bogdan Dumitriu
 */
public class DirListingTest extends TestCase
{
    private SVNRAStorage _storage;

    @Override
    protected void setUp() throws Exception
    {
        _storage = new SVNRAStorage("svn://localhost:10000/", "gwuser", "gwpassword");
        //_storage = new SVNRAStorage("https://svn.cs.uu.nl:12443/repos/test-wiki/", "gwstorage", "12345");
    }

    public void testBasicDirListing() throws Exception
    {
    	HashSet<String> result = new HashSet<String>(); 
        HashSet<String> reference = new HashSet<String>();

        Iterator<String> it = _storage.getDirListing("branches/dirListing/");
        while ( it.hasNext() )
            assertTrue("Duplicates in the returned dirlist", result.add(it.next()));

        reference.add("/branches/dirListing/dir1");
        reference.add("/branches/dirListing/dir2");
        reference.add("/branches/dirListing/dir3");
        reference.add("/branches/dirListing/f1");
        reference.add("/branches/dirListing/f2");

        assertEquals(reference, result);
    }
    
    public void testRecurseDirListing() throws Exception
    {
        HashSet<String> result = new HashSet<String>(); 
        HashSet<String> reference = new HashSet<String>();

        Iterator<String> it = _storage.getDirListing("branches/dirListing", true);
        while ( it.hasNext() )
            assertTrue("Duplicates in the returned dirlist", result.add(it.next()));

        reference.add("/branches/dirListing/dir1");
        reference.add("/branches/dirListing/dir2");
        reference.add("/branches/dirListing/dir3");
        reference.add("/branches/dirListing/f1");
        reference.add("/branches/dirListing/f2");
        reference.add("/branches/dirListing/dir1/dir11");
        reference.add("/branches/dirListing/dir1/dir12");
        reference.add("/branches/dirListing/dir1/f11");
        reference.add("/branches/dirListing/dir1/dir11/dir111");
        reference.add("/branches/dirListing/dir1/dir11/f111");
        reference.add("/branches/dirListing/dir1/dir11/dir111/f1111");
        reference.add("/branches/dirListing/dir1/dir12/dir121");
        reference.add("/branches/dirListing/dir1/dir12/dir122");
        reference.add("/branches/dirListing/dir1/dir12/f121");
        reference.add("/branches/dirListing/dir1/dir12/dir122/f1221");
        reference.add("/branches/dirListing/dir2/f21");
        reference.add("/branches/dirListing/dir3/dir31");
        reference.add("/branches/dirListing/dir3/dir31/dir311");
        reference.add("/branches/dirListing/dir3/dir31/f311");
        reference.add("/branches/dirListing/dir3/dir31/f312");
        reference.add("/branches/dirListing/dir3/dir31/dir311/dir3111");
        reference.add("/branches/dirListing/dir3/dir31/dir311/dir3111/dir31111");

        assertEquals(reference, result);
    }
    
    public void testRecurseDirListingEdited() throws Exception
    {
        HashSet<String> result = new HashSet<String>(); 
        HashSet<String> reference = new HashSet<String>();
        
        reference.add("/branches/dirListing/dir1/f11");
        reference.add("/branches/dirListing/dir1/dir13");
        reference.add("/branches/dirListing/dir1/f12");
        
        _storage.makeDirectory("branches/dirListing/dir1/dir13");      
        _storage.storeFile("branches/dirListing/dir1/f12").close();
        _storage.deleteFile("branches/dirListing/dir1/dir11", false);
        _storage.deleteFile("branches/dirListing/dir1/dir12", false);

        Iterator<String> it = _storage.getDirListing("branches/dirListing/dir1", true);
//        System.out.println("recusredirlisting");
        while (it.hasNext())
        {
            String entry = it.next();
//            System.out.println(entry);
            assertTrue("Duplicates in the returned dirlist", result.add(entry));
        }
        
        assertEquals(reference, result);
    }
    

    
    public void testRevisionDirListing() throws Exception
    {
        Map<String, StorageDirEntry> entries = _storage.getDirListing("/branches/status/somedir", 3, true);
        Map<String, StorageDirEntry> reference = new HashMap<String, StorageDirEntry>();
        TestStorageDirEntry refSDE = new TestStorageDirEntry(
        		new Date(1128945921462l), 2l, "gwuser", true, false);
        reference.put("/branches/status/somedir/een",refSDE);
        reference.put("/branches/status/somedir/twee",refSDE);
        reference.put("/branches/status/somedir/drie",refSDE);
        reference.put("/branches/status/somedir/vier",refSDE);
        
        assertEquals(reference, entries);
    }
}
