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

public class RevertFileSimpleTest extends TestCase
{
    private static final String USER="gwuser";
    private static final String PASSWORD="gwpassword";
    private static final String URL="svn://localhost:10000/branches/revertFile/";
    private SVNRAStorage _storage;
    
    protected void setUp() throws Exception
    {
        _storage = new SVNRAStorage(URL,USER,PASSWORD);
    }
    

    
    //Reverting a locally created file
    //This should remove all pressence of this file
    //from the tree
    public void testRevertFileString_newfile()
    {
        try
        {
            BufferedOutputStream out = new BufferedOutputStream(_storage.storeFile("notevergoingtoexist"));
            byte[] buffer = "test".getBytes();
            out.write(buffer,0,buffer.length);
            out.close();
            _storage.revertFile("notevergoingtoexist");
            _storage.getFile("notevergoingtoexist");
            fail("File shouldn't exist but does");
            
        }
        catch(Exception e)
        {
           //System.out.println(e);
        }
        
    }
    
    //Reverting a child from a deleted directory
    // revert \
    //        + child1
    //        + child2
    //        + child3
    //The directory revert will be deleted
    //Then we revert the child2
    //Child1 and 3 should stay deleted
    public void testRevertFileString_removeddir()
    {
        try
        {
            _storage.deleteFile("revert",false);
            if(_storage.fileExists("revert/child2"))
            {
                fail("revert/child2 shouldn't exist anymore");
            }
            if(_storage.fileExists("revert"))
            {
                fail("revert shouldn't exist anymore");
            }
            _storage.revertFile("revert/child2");
            if(!_storage.fileExists("revert/child2"))
            {
                fail("revert/child2 should exist again");
            }
            if(!_storage.fileExists("revert"))
            {
                fail("revert should exist again");
            }
            if(_storage.fileExists("revert/child1"))
            {
                fail("revert/child1 shouldn't exist");
            }
            
            
        }
        catch(Exception e)
        {
           fail(e.toString());
           //StackTraceElement[] elem = e.getStackTrace();
           //int length = elem.length;
           //for(int i = 0; i < length; i++)
           //{
           //    System.out.println(elem[i]);
           //}
        }
        
    }    
}
