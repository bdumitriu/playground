package gw.users.acl;

import gw.users.User;

import java.io.*;
import java.util.*;

import junit.framework.*;
import gw.storage.*;

/**
 * Unit test for StorageACLAdapter.
 */
public class StorageACLAdapterTest extends TestCase {
    /**
     * Construct a new Testcase with the given name
     * @param name
     */
    public StorageACLAdapterTest(final String name)  {
        super(name);
    }


    /**
     * Initialize for each test
     */ 
    protected void setUp() throws IOException {
    }
	
	/**
	 * Destruct for each test
	 */
	protected void tearDown() throws IOException {
	}
    
    public void testAddRemove() throws StorageException  {
        Storage storage = new ACLMockStorage();
        StorageACLAdapter aclDef   = new StorageACLAdapter( "/aclTest", storage );
        User user       = new User( "SpefdeBever" );
        
        aclDef.addACLRight(GwACLPermissions.READ_PERMISSION, user.getId());
        assertTrue(aclDef.permissionEntryExists(GwACLPermissions.READ_PERMISSION));
        List listIncludingUser = aclDef.getUsersForPermission(GwACLPermissions.READ_PERMISSION);
        assertTrue(listIncludingUser.contains(user.getId()));
        
        aclDef.removeACLRight(GwACLPermissions.READ_PERMISSION, user.getId());
        assertFalse(aclDef.permissionEntryExists(GwACLPermissions.READ_PERMISSION));
        List listExcludingUser = aclDef.getUsersForPermission(GwACLPermissions.READ_PERMISSION);
        assertTrue(listExcludingUser == null);
    }
    
    public void testDifferentPermissions( ) throws StorageException {
        Storage storage = new ACLMockStorage();
        StorageACLAdapter aclDef   = new StorageACLAdapter( "/aclTest", storage );
        User user       = new User( "SpefdeBever" );
		
		aclDef.addACLRight(GwACLPermissions.READ_PERMISSION, user.getId());
		assertTrue(aclDef.permissionEntryExists(GwACLPermissions.READ_PERMISSION));
		List listNotIncludingUser = aclDef.getUsersForPermission(GwACLPermissions.WRITE_PERMISSION);
        assertTrue(listNotIncludingUser == null);
        
        aclDef.addACLRight(GwACLPermissions.WRITE_PERMISSION, user.getId());
        assertTrue(aclDef.permissionEntryExists(GwACLPermissions.WRITE_PERMISSION));
        List listIncludingUser = aclDef.getUsersForPermission(GwACLPermissions.WRITE_PERMISSION);
        assertTrue(listIncludingUser.contains(user.getId())); 	
    }
    
    public void testRemovalOnePermission( ) throws StorageException {
        Storage storage = new ACLMockStorage();
        StorageACLAdapter aclDef   = new StorageACLAdapter( "/aclTest", storage );
        User user       = new User( "SpefdeBever" );
    	
    	// Give user some permissions
		aclDef.addACLRight(GwACLPermissions.READ_PERMISSION, user.getId());
		assertTrue(aclDef.permissionEntryExists(GwACLPermissions.READ_PERMISSION));
		aclDef.addACLRight(GwACLPermissions.WRITE_PERMISSION, user.getId());
		assertTrue(aclDef.permissionEntryExists(GwACLPermissions.WRITE_PERMISSION));
    	
    	// Remove some permissions of him
		aclDef.removeACLRight(GwACLPermissions.READ_PERMISSION, user.getId());
		List listExcludingUser = aclDef.getUsersForPermission(GwACLPermissions.READ_PERMISSION);
        assertTrue(listExcludingUser == null);
		
		// The user should still have write permission
		List listIncludingUser = aclDef.getUsersForPermission(GwACLPermissions.WRITE_PERMISSION);
        assertTrue(listIncludingUser != null);
		assertTrue(listIncludingUser.contains(user.getId()));	
    }
   
 	public void testMultipleReadWritePermissions() throws StorageException {
        Storage storage = new ACLMockStorage();
        StorageACLAdapter aclDef   = new StorageACLAdapter( "/aclTest", storage );
        User user1      = new User( "SpefdeBever" );
        User user2      = new User( "FoxDeVos" );
		
        List list;
		
        //add users read and write permissions
		aclDef.addACLRight(GwACLPermissions.READ_PERMISSION, user1.getId());
		aclDef.addACLRight(GwACLPermissions.WRITE_PERMISSION, user1.getId());
		//add spef read and write permissions
		aclDef.addACLRight(GwACLPermissions.READ_PERMISSION, user2.getId());
		aclDef.addACLRight(GwACLPermissions.WRITE_PERMISSION, user2.getId());
		
		//Revoke read perm from user, write perm should still be there
		aclDef.removeACLRight(GwACLPermissions.READ_PERMISSION, user1.getId());
		list = aclDef.getUsersForPermission( GwACLPermissions.READ_PERMISSION);
		assertFalse( list.contains( user1.getId() ) );
		list = aclDef.getUsersForPermission( GwACLPermissions.WRITE_PERMISSION );
		assertTrue( list.contains( user1.getId() ) );
		
		// Revoke write perm from spef, read perm should still be there
		aclDef.removeACLRight(GwACLPermissions.WRITE_PERMISSION, user2.getId());
		list = aclDef.getUsersForPermission( GwACLPermissions.WRITE_PERMISSION);
		assertFalse( list.contains( user2.getId() ) );
		list = aclDef.getUsersForPermission( GwACLPermissions.READ_PERMISSION );
		assertTrue( list.contains( user2.getId() ) );
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
        TestSuite suite = new TestSuite(StorageACLAdapterTest.class);
        return suite;
    }
}
