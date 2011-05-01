package gw.storage;

import gw.users.*;
import gw.users.acl.*;
import junit.framework.*;


/**
 * Functional test of ACLs.
 */
public class SecuredStorageTest extends TestCase {
    /**
     * Construct a new Testcase with the given name
     * @param name
     */
    public SecuredStorageTest(final String name) {
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
     * Tests if the Secured Storage provides its basic functions.
     */
    public void testScenario() throws StorageException {
        ACLMockStorage storage = new ACLMockStorage();
        User owner = new User("Jan");
        StorageACLAdapterFactory saaf = new StorageACLAdapterFactory(storage);
        TestSecuredStorage secured = new TestSecuredStorage(storage, owner);
        
        ACLAdapter aclGroups = saaf.getACLDefinition(new FilePathACLResource("/Groups"));
        ACLAdapter aclAll = saaf.getACLDefinition(new FilePathACLResource("/Groups/All"));
        aclGroups.addACLRight(GwACLPermissions.BROWSE_PERMISSION, "Jan");
        aclGroups.addACLRight(GwACLPermissions.READ_PERMISSION, "Jan");
        aclAll.addACLRight(GwACLPermissions.READ_PERMISSION, "Jan");

        secured.getDirListing("/Groups");
        secured.getFile("/Groups/All");
        
        secured.setOwner(new User("Miep"));
        
        try { secured.getDirListing("/Groups"); assertTrue(false); }
        catch(InsufficientStorageAccessPermissionsException e) {
            assertTrue(true);
        }
        
        try { secured.getFile("/Groups/All"); assertTrue(false); }
        catch(InsufficientStorageAccessPermissionsException e) {
            assertTrue(true);
        }
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
        TestSuite suite = new TestSuite(SecuredStorageTest.class);
        return suite;
    }
    
    /**
     * Test secured storage class which can change its owner in mid-flight.
     * It would be hard to test this functionality otherwise.
     */
    private class TestSecuredStorage extends SecuredStorage {
        public TestSecuredStorage(Storage delegate, User owner) {
            super(delegate, owner);
        }
        
        public void setOwner(User user) {
            _owner = user;
        }
    }
}
