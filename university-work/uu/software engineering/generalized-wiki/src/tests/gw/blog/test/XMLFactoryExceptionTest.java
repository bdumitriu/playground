package gw.blog.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import gw.blog.XMLFactoryException;

public class XMLFactoryExceptionTest extends TestCase {

	public XMLFactoryExceptionTest(final String name) {
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
        TestSuite suite = new TestSuite(XMLFactoryExceptionTest.class);
        return suite;
    }
	
    public void testXMLFactoryExceptions()
    {
    	Exception e0 = new XMLFactoryException();
    	Exception e1 = new XMLFactoryException("test");
    	Exception e2 = new XMLFactoryException("test",new NullPointerException("nullP"));
    	Exception e3 = new XMLFactoryException(new NullPointerException("nullPoint"));
    	
    	assertEquals("1",null,e0.getMessage());
    	assertEquals("2","test",e1.getMessage());   	
    	assertEquals("3","test",e2.getMessage());
    	assertEquals("4",new NullPointerException("nullP").getMessage(),e2.getCause().getMessage());
    	assertEquals("6",new NullPointerException("nullPoint").getMessage(),e3.getCause().getMessage());
    	
    }
    
}
