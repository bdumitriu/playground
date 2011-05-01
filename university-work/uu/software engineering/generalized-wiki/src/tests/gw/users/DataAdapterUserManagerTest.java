package gw.users;

import junit.framework.*;

public class DataAdapterUserManagerTest extends TestCase {
	private DataAdapterUserManager _daum;
	private MockUserDataAdapter _muda;
	
	public DataAdapterUserManagerTest(String name) {
		super(name);
	}
	
	protected void setUp() {
		_muda = new MockUserDataAdapter();
		_daum = new DataAdapterUserManager(_muda);
	}
	
	protected void tearDown() {
		_muda = null;
		_daum = null;
	}
	
	public void testCreateUser() {
		try {
			_daum.createUser("Alice", "alice@mail.com", "iLoveBob");
		}
		catch(Exception e) {
			assertTrue(false);
		}
		
		assertTrue(_daum.existUser("Alice"));
		assertEquals(_daum.getUser("Alice").getId(), "Alice");
	}
	
	public void testUserList() {
		_daum.createUser("Alice", "alice@mail.com", "iLoveBob");
		
		assertTrue(_daum.getUsers().size() == 1);
		
		_daum.createUser("Bob", "bob@mail.com", "iDespiseAlice");
		
		assertTrue(_daum.getUsers().size() == 2);
	}

}
