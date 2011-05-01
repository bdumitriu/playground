package gw.users.acl;


import java.util.*;
import junit.framework.*;


/**
 * Tests if the permission and actions classes are actually working.
 */
public class ActionsTest extends TestCase {
    public ActionsTest(final String name) {
        super(name);
    }
    
    /**
     * Tests if the view action is defined.
     */
    public void testViewActionDefined()  {
        ACLResourceAction action = GwACLResourceActions.RETRIEVE_ACTION;
        List list = action.getRequiredPermissions();
        
        assertFalse(list.isEmpty());
        assertFalse(list.size() > 1);
        
        ACLPermission permission = (ACLPermission) list.get(0);
        String permName = permission.getIdentifier();
        
        assertFalse(permName == null);
        assertFalse(permName.trim().equals(""));
        assertTrue(permName.equals(((ACLPermission) GwACLResourceActions.RETRIEVE_ACTION.getRequiredPermissions().get(0)).getIdentifier()));        
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
        TestSuite suite = new TestSuite(ActionsTest.class);
        return suite;
    }
}
