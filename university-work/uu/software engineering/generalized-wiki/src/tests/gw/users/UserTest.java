package gw.users;

import gw.users.User;
import junit.framework.*;

/**
 * Unittests the gw.users.User class
 * .. which contains pretty basic accesor methods, so there is little need to test them.
 */
public class UserTest extends TestCase {

    User _user = null;

    /**
     * Construct a new Testcase with the given name
     * @param name
     */
    public UserTest(final String name) {

        super(name);
    }
   
   /**
    * Initialize for each test
    */ 
    protected void setUp( ) {
        _user = new User( "Foo", "foo@bar.com");
    }
    
    /**
     * Destruct for each test
     */
    protected void tearDown( ) {
        
        _user = null;
    }
    
    /**
     * Test if a property of an user can be set with an null key
     */
    public void testNullKeyProperty( ) {        
        try {
            _user.setProperty( null, "foo");
            fail( "IllegalArgumentException should be generated");
        
        }
        catch (IllegalArgumentException iexp) {}  
    }
    
    public void testEmailFunctions() {
    	_user.setEmail("me@home.com");
    	
    	assertEquals(_user.getEmail(), "me@home.com");
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

      TestSuite suite = new TestSuite(UserTest.class);
      return suite;
    }
}
