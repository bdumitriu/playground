package gw.render.test;

import gw.render.StylesheetApplyException;

import junit.framework.TestCase;

public class StylesheetApplyExceptionTest extends TestCase {
	
    public StylesheetApplyExceptionTest(final String name) {
        super(name);
    }
    
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(StylesheetApplyExceptionTest.class);
    }
	
    public void testThrow()
    {
    	try
    	{
    		throw new StylesheetApplyException();
    	}
    	catch(StylesheetApplyException sae)
    	{
    		assertTrue(true);
    	}
    }
    
    public void testThrowWithParam()
    {
    	try
    	{
    		throw new StylesheetApplyException("Throwing a stampler to you !");
    	}
    	catch(StylesheetApplyException sae)
    	{
    		assertEquals(sae.getMessage(), "Throwing a stampler to you !");
    	}
    }

    public void testThrowWithPThrowable()
    {
    	try
    	{
    		throw new StylesheetApplyException(new StylesheetApplyException("Throwing a stampler to you !"));
    	}
    	catch(StylesheetApplyException sae)
    	{
    		assertEquals(sae.getMessage(), "gw.render.StylesheetApplyException: Throwing a stampler to you !");
    	}
    }
    
    public void testThrowWithParamAndThrowable()
    {
    	try
    	{
    		throw new StylesheetApplyException("Throwing a stampler to you !", new StylesheetApplyException());
    	}
    	catch(StylesheetApplyException sae)
    	{
    		assertEquals(sae.getMessage(), "Throwing a stampler to you !");
    	}
    }
}
