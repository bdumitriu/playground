package gw.users.acl;

import gw.users.*;
import gw.storage.*;
import junit.framework.*;

/**
 * Functional test of ACLs.
 */
public class FunctionalACLTest extends TestCase {
    /**
     * Construct a new Testcase with the given name
     * @param name
     */
    public FunctionalACLTest(final String name) {
        super(name);
    }

    /**
     * Initialize for each test
     */ 
    protected void setUp() {
    }
    
    /**
     * Destruct for each test
     */
    protected void tearDown() {
    }

    /**
     * Tests if the ACL does deny access to the resource.
     */
    public void testScenario() throws StorageException {
        Storage storage = new ACLMockStorage();
        StorageACLAuthorizer authorizer = new StorageACLAuthorizer(storage);
        
        StorageACLAdapterFactory saaf = new StorageACLAdapterFactory(storage);
        
        ACLAdapter aclFile1 = saaf.getACLDefinition(new FilePathACLResource("/aclTest"));
        ACLAdapter aclFile2 = saaf.getACLDefinition(new FilePathACLResource("/aclTest/File2"));
        ACLAdapter aclFile3 = saaf.getACLDefinition(new FilePathACLResource("/aclTest/File3"));
        
        aclFile1.addACLRight(GwACLPermissions.BROWSE_PERMISSION, "All");
        aclFile1.addACLRight(GwACLPermissions.READ_PERMISSION, "NoBody");
        aclFile2.addACLRight(GwACLPermissions.READ_PERMISSION, "Singleton");
        aclFile2.addACLRight(GwACLPermissions.READ_PERMISSION, "Miep");
        
        assertTrue(authorizer.hasAccess(new FilePathACLResource("/aclTest"), GwACLResourceActions.BROWSE_ACTION, new User("Jan")));
        assertTrue(authorizer.hasAccess(new FilePathACLResource("/aclTest"), GwACLResourceActions.BROWSE_ACTION, new User("Miep")));
        assertTrue(authorizer.hasAccess(new FilePathACLResource("/aclTest"), GwACLResourceActions.BROWSE_ACTION, new User("Klaas")));
        
        assertTrue(authorizer.hasAccess(new FilePathACLResource("/aclTest/File2"), GwACLResourceActions.RETRIEVE_ACTION, new User("Jan")));
        assertTrue(authorizer.hasAccess(new FilePathACLResource("/aclTest/File2"), GwACLResourceActions.RETRIEVE_ACTION, new User("Miep")));
        assertFalse(authorizer.hasAccess(new FilePathACLResource("/aclTest/File2"), GwACLResourceActions.RETRIEVE_ACTION, new User("Klaas")));
        
        assertFalse(authorizer.hasAccess(new FilePathACLResource("/aclTest/File3"), GwACLResourceActions.RETRIEVE_ACTION, new User("Jan")));
        assertFalse(authorizer.hasAccess(new FilePathACLResource("/aclTest/File3"), GwACLResourceActions.RETRIEVE_ACTION, new User("Miep")));
        assertFalse(authorizer.hasAccess(new FilePathACLResource("/aclTest/File3"), GwACLResourceActions.RETRIEVE_ACTION, new User("Klaas")));
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
        TestSuite suite = new TestSuite(FunctionalACLTest.class);
        return suite;
    }
}
