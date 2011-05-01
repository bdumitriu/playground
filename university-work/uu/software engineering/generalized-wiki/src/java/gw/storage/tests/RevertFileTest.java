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

public class RevertFileTest extends TestCase
{
    private static final String USER="gwuser";
    private static final String PASSWORD="gwpassword";
    private static final String URL="svn://localhost:10000/branches/revertFile/";
    private SVNRAStorage _storage;
    
    protected void setUp() throws Exception
    {
        _storage = new SVNRAStorage(URL,USER,PASSWORD);
    }
    
    //revert1.txt
    //20 = 1
    //21 = 1\n2
    //22 = 1\n2\n3
    
    
    public void testRevertFileStringLong_singlefile()
    {
        try
        {
            BufferedReader in = new BufferedReader(new InputStreamReader(_storage.getFile("revert1.txt")));
            String test = "";
            String tmp = "";
            while((tmp = in.readLine()) != null)
            {
                test += tmp + "\n";
            }
            assertEquals("1\n2\n3\n",test);
            _storage.revertFile("revert1.txt",21);
            _storage.commit("reverting revert1.txt to revision 21");
            in = new BufferedReader(new InputStreamReader(_storage.getFile("revert1.txt")));
            test = "";
            tmp = "";
            while((tmp = in.readLine()) != null)
            {
                test += tmp + "\n";
            }
            assertEquals("1\n2\n",test);
        }
        catch(Exception e)
        {
            fail(e.toString());
        }
    }
    
    public void testRevertFileString_dirOverFile()
    {
        try
        {
            _storage.revertFile("old_dir",24);
            
            fail("should not be able to revert a dir over a file");
            
        }
        catch(Exception e)
        {
           System.out.println(e);
        }
        
    }
    

}

