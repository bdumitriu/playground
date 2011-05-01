package gw.storage;

import java.io.*;
import gw.storage.javasvn.SVNRAStorage;
import junit.framework.TestCase;

public class GetFileTest extends TestCase
{
    private static final String USER="gwuser";
    private static final String PASSWORD="gwpassword";
    private static final String URL="svn://localhost:10000/branches/getFile/";
    private SVNRAStorage _storage;
    
    protected void setUp() throws Exception
    {
        _storage = new SVNRAStorage(URL,USER,PASSWORD);
    }
    
    public void testGetFileString_unexisting()
    {
        try
        {
            _storage.getFile("nonexistantfile");
            fail("No error returned when requesting a non existing file");
        }
        catch(Exception e)
        {
            assertTrue(true);
        }
    }
    
    public void testGetFileString_existing() throws IOException, StorageException
    {
        InputStream in = _storage.getFile("test_string.txt");
        compareFileContents("test string", in);
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
    
    public void testGetFileStringLong_olderversion() throws IOException, StorageException
    {
        InputStream in = _storage.getFile("revision.txt");
        compareFileContents("second version of file", in);
        in = _storage.getFile("revision.txt",18);
        compareFileContents("first version of file",in);
    }
    
    public void testGetFileString_newfile() throws StorageException, IOException
    {
        BufferedOutputStream out = new BufferedOutputStream(_storage.storeFile("notevergoingtoexist"));
        byte[] buffer = "test".getBytes();
        out.write(buffer,0,buffer.length);
        out.close();
        InputStream in = _storage.getFile("notevergoingtoexist");
        compareFileContents("test", in);
    }
}
