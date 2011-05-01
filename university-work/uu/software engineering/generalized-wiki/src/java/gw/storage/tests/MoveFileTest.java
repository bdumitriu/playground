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

public class MoveFileTest extends TestCase
{
    private static final String USER="gwuser";
    private static final String PASSWORD="gwpassword";
    private static final String URL="svn://localhost:10000/branches/copyFile/";
    private SVNRAStorage _storage;
    
    protected void setUp() throws Exception
    {
        _storage = new SVNRAStorage(URL,USER,PASSWORD);
    }
    
    //Move a locolly added file without force
    //should not move the file
    public void testMoveFile_StringStringbool_singlefile_noforce()
    {
        try
        {
            
            BufferedOutputStream out = new BufferedOutputStream(_storage.storeFile("locallyadded"));
            byte[] buffer = "test".getBytes();
            out.write(buffer,0,buffer.length);
            out.close();
            _storage.moveFile("locallyadded","single.copy",false);
            fail("should not be able to copy locally created files");
            
            
            
            
            
        }
        catch(Exception e)
        {
            //System.out.println(e);
        }
        
    }
    
//  Move a locolly added file without force
    //should not move the file
    public void testMoveFile_StringStringbool_singlefile()
    {
        try
        {
            
            BufferedOutputStream out = new BufferedOutputStream(_storage.storeFile("locallyadded"));
            byte[] buffer = "test".getBytes();
            out.write(buffer,0,buffer.length);
            out.close();
            _storage.moveFile("locallyadded","locallyadded.copy",true);
            InputStream in = _storage.getFile("locallyadded.copy");
            compareFileContents("test", in);
            assertTrue(!_storage.fileExists("locallyadded"));
            
            
            
            
            
        }
        catch(Exception e)
        {
            System.out.println(e);
            fail(e.toString());
        }
        
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
}
