package gw.users;

import gw.GwContext;
import gw.GwSessionContext;
import gw.MockServletContext;
import gw.storage.MockStorage;
import gw.storage.StorageException;
import gw.util.DigestAuthenticationUtility;

import junit.framework.*;

public class StorageUserDataAdapterTest extends TestCase {
	MockServletContext _msc = new MockServletContext();
	GwContext _gc = new GwContext(_msc);
	GwSessionContext _gsc = new GwSessionContext(_gc);
	private StorageUserDataAdapter _suda;
	
	public StorageUserDataAdapterTest(String name) {
		super(name);
	}
	
	public void setUp() {
		try {
			_suda = new StorageUserDataAdapter("/Users", _gsc.getUnsecuredStorage());
		}
		catch(StorageException se) {}
	}
	
	public void tearDown() {
		_suda = null;
	}
	
	public void testCreateExistsListDelete() {
		User testUser = new User("Alice");
		
		_suda.create(testUser, "iLoveBob");
		
		assertTrue(_suda.exists(testUser));
		assertEquals(((User) _suda.getAllUsers().get(0)).getId(), testUser.getId());
		
		_suda.create(new User("Bob"), "iLoveAlice");
		
		assertTrue(_suda.getAllUsers().size() == 2);
		
		_suda.delete(testUser);
		
		assertFalse(_suda.exists(testUser));
	}
	
	public void testPasswordOps() {
		User testUser = new User("Alice");
		
		_suda.create(testUser, "iLoveBob");
		
		assertEquals(_suda.getPassword(testUser), 
				DigestAuthenticationUtility.getPasswordHash(testUser.getId(), "iLoveBob"));
		
		_suda.setPassword(testUser, "iLoveCharleneNow");
		
		assertEquals(_suda.getPassword(testUser),
				DigestAuthenticationUtility.getPasswordHash(testUser.getId(), "iLoveCharleneNow"));
	}
	
	public void testLoadStore() {
		User testUser = new User("Alice");
		
		_suda.create(testUser, "iLoveBob");
		
		try {
			_suda.load(testUser);
		}
		catch(IllegalStateException ise) {
			fail("There should be no exceptions.");
		}
		
		try {
			_suda.load(new User("Bob"));
			fail("An exception should have occurred");
		}
		catch(NoSuchUserException nsue) {
			
		}	
		
		try {
			_suda.store(testUser);
		}
		catch(IllegalStateException ise) {
			fail("An exception should have occurred");
		}
	}
}
