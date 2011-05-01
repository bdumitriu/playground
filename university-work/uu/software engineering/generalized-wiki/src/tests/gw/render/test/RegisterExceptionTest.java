package gw.render.test;

import gw.render.RegisterException;

import junit.framework.TestCase;

public class RegisterExceptionTest extends TestCase {
	
    public RegisterExceptionTest(final String name) {
        super(name);
    }
    
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(RegisterExceptionTest.class);
    }
	
    public void testThrow()
    {
    	try
    	{
    		throw new RegisterException();
    	}
    	catch(RegisterException re)
    	{
    		assertTrue(true);
    	}
    }
    
    public void testThrowWithParam()
    {
    	try
    	{
    		throw new RegisterException("Throwing a stampler to you !");
    	}
    	catch(RegisterException re)
    	{
    		assertEquals(re.getMessage(), "Throwing a stampler to you !");
    	}
    }

    public void testThrowWithPThrowable()
    {
    	try
    	{
    		throw new RegisterException(new RegisterException("Throwing a stampler to you !"));
    	}
    	catch(RegisterException re)
    	{
    		assertEquals(re.getMessage(), "gw.render.RegisterException: Throwing a stampler to you !");
    	}
    }
    
    public void testThrowWithParamAndThrowable()
    {
    	try
    	{
    		throw new RegisterException("Throwing a stampler to you !", new RegisterException());
    	}
    	catch(RegisterException re)
    	{
    		assertEquals(re.getMessage(), "Throwing a stampler to you !");
    	}
    }
}
