package gw.render.test;

import gw.render.ParserFactoryException;

import junit.framework.TestCase;

public class ParserFactoryExceptionTest extends TestCase {
	
    public ParserFactoryExceptionTest(final String name) {
        super(name);
    }
    
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(ParserFactoryExceptionTest.class);
    }
    
    public void testThrowWithParam()
    {
    	try
    	{
    		throw new ParserFactoryException("Throwing a stampler to you !");
    	}
    	catch(ParserFactoryException pfe)
    	{
    		assertEquals(pfe.getMessage(), "Throwing a stampler to you !");
    	}
    }
}
