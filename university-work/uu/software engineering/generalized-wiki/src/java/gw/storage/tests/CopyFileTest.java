package gw.storage.tests;

import java.io.*;
import java.util.*;

import gw.storage.*;

import org.tmatesoft.svn.core.*;
import org.tmatesoft.svn.core.auth.*;
import org.tmatesoft.svn.core.io.*;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.wc.*;
import gw.storage.javasvn.SVNRAStorage;
import junit.framework.TestCase;

public class CopyFileTest extends TestCase
{
    private static final String USER="gwuser";
    private static final String PASSWORD="gwpassword";
    private static final String URL="svn://localhost:10000/branches/copyFile/";
    private SVNRAStorage _storage;
    
    protected void setUp() throws Exception
    {
        _storage = new SVNRAStorage(URL,USER,PASSWORD);
    }
    
    //Copy a single file that was already in the repo
    //and has no local changes
    //Not commiting the change
    //So testing the copyFile & getRealPath in a sence
    public void testCopyFile_StringString_singlefile_nocommit()
    {
        try
        {
            
            InputStream in = _storage.getFile("single.file");
            compareFileContents("single", in);
            _storage.copyFile("single.file","single.copy");
            //_storage.commit("copy single.file to single.copy");
            
            assertTrue(_storage.fileExists("single.copy"));
            
            in = _storage.getFile("single.copy");
            
            compareFileContents("single", in);
            
            
            
        }
        catch(Exception e)
        {
           fail(e.toString());
        }
        
    }
    
//  Copy a single file that was already in the repo
    //and has no local changes
    //Commiting and seeing if the copied file is correct
    public void testCopyFile_StringString_singlefile()
    {
        try
        {
            
            InputStream in = _storage.getFile("single.file");
            compareFileContents("single", in);
            _storage.copyFile("single.file","single.copy1");
            _storage.commit("copy single.file to single.copy1");
            
            assertTrue(_storage.fileExists("single.copy1"));
            
            in = _storage.getFile("single.copy1");
            
            compareFileContents("single", in);
            
            
            
        }
        catch(Exception e)
        {
           fail(e.toString());
        }
        
    }
    
    //Copy a single file that is locally created
    public void testCopyFile_StringString_newfile()
    {
        try
        {
            
            BufferedOutputStream out = new BufferedOutputStream(_storage.storeFile("locallyadded"));
            byte[] buffer = "test".getBytes();
            out.write(buffer,0,buffer.length);
            out.close();
            _storage.copyFile("locallyadded","single.copy");
            fail("should not be able to copy locally created files");
            
            
            
            
            
        }
        catch(Exception e)
        {
           
        }
        
    }
    
    //Copying a locally copied file
    public void testCopyFile_StringString_copyofcopy()
    {
        try
        {
            
            InputStream in = _storage.getFile("single.file");
            compareFileContents("single", in);
            
            _storage.copyFile("single.file","single.copy");
            in = _storage.getFile("single.copy");
            compareFileContents("single", in);
            _storage.copyFile("single.copy","single.copy2");
            in = _storage.getFile("single.copy2");
            
            _storage.commit("copy single.file to single.copy and copy single.copy to single.copy2");
            
            assertTrue(_storage.fileExists("single.copy"));
            assertTrue(_storage.fileExists("single.copy2"));
            
            in = _storage.getFile("single.copy2");            
            compareFileContents("single", in);
            
            
            
        }
        catch(Exception e)
        {
           fail(e.toString());
        }
        
    }
    
    //Copy a single file that was already in the repo
    //and has no local changes into a nonexisting dir
    public void testCopyFile_StringString_fileintodir_nocommit()
    {
        try
        {
            
            InputStream in = _storage.getFile("single.file");
            compareFileContents("single", in);
            _storage.copyFile("single.file","test_dir/");
            //_storage.commit("copy single.file to test_dir/single.file");
            
            assertTrue(_storage.fileExists("test_dir/single.file"));
            
            in = _storage.getFile("test_dir/single.file");
            
            compareFileContents("single", in);
            
            
            
        }
        catch(Exception e)
        {
           fail(e.toString());
        }
        
    }
    
    //Copy a single file that was already in the repo
    //and has no local changes into a nonexisting dir
    //Commiting and seeing if the copied file is correct
    //this fails for now because the order of processing
    //commititems is wrong
    /*
    public void testCopyFile_StringString_fileintodir()
    {
        try
        {
            
            InputStream in = _storage.getFile("single.file");
            compareFileContents("single", in);
            _storage.copyFile("single.file","test_dir2/");
            _storage.commit("copy single.file to test_dir2/single.file");
            
            assertTrue(_storage.fileExists("test_dir2/single.file"));
            
            in = _storage.getFile("test_dir2/single.file");
            
            compareFileContents("single", in);
            
            
            
        }
        catch(Exception e)
        {
           fail(e.toString());
        }
        
    }*/
    
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
}
