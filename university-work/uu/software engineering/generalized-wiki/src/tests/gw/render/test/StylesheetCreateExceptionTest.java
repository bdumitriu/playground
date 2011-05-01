package gw.render.test;

import gw.render.StylesheetCreateException;

import junit.framework.TestCase;

public class StylesheetCreateExceptionTest extends TestCase {
	
    public StylesheetCreateExceptionTest(final String name) {
        super(name);
    }
    
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(StylesheetCreateExceptionTest.class);
    }
	
    public void testThrow()
    {
    	try
    	{
    		throw new StylesheetCreateException();
    	}
    	catch(StylesheetCreateException sce)
    	{
    		assertTrue(true);
    	}
    }
    
    public void testThrowWithParam()
    {
    	try
    	{
    		throw new StylesheetCreateException("Throwing a stampler to you !");
    	}
    	catch(StylesheetCreateException sce)
    	{
    		assertEquals(sce.getMessage(), "Throwing a stampler to you !");
    	}
    }

    public void testThrowWithPThrowable()
    {
    	try
    	{
    		throw new StylesheetCreateException(new StylesheetCreateException("Throwing a stampler to you !"));
    	}
    	catch(StylesheetCreateException sce)
    	{
    		assertEquals(sce.getMessage(), "gw.render.StylesheetCreateException: Throwing a stampler to you !");
    	}
    }
    
    public void testThrowWithParamAndThrowable()
    {
    	try
    	{
    		throw new StylesheetCreateException("Throwing a stampler to you !", new StylesheetCreateException());
    	}
    	catch(StylesheetCreateException sce)
    	{
    		assertEquals(sce.getMessage(), "Throwing a stampler to you !");
    	}
    }
}
