package gw.blog.test;

import gw.blog.FileUtils;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class FileUtilsTest extends TestCase{

	public FileUtilsTest(final String name) {
        super(name);
    }
    
    /**
     * Run this testcase in textmode
     * @param args
     */
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(suite());
    }
    
    /**
     * Create a TestSuite based on this TestCase
     * @return the generated TestSuite
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(FileUtilsTest.class);
        return suite;
    }
    
    /**
     *	 Test the function dirname()
     */
    public void testdirname()
    {
    	String dirName = FileUtils.dirname("/test/test2/test3/file.xml");
    	assertEquals("dir not the same name",dirName,"/test/test2/test3");
    }
	
    /**
     *	 Test the function basename()
     */
    public void testbasename()
    {
    	String baseName = FileUtils.basename("/test/test2/test3/file.xml");
    	assertEquals("file not the same name",baseName,"file.xml");
    }
    
}