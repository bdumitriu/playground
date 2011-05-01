package gw.users;

import java.util.List;

public class MockUserManager implements UserManager {
	private MockUserDataAdapter _muda;
	
	public MockUserManager() {
		_muda = new MockUserDataAdapter();
	}
	
	public User createUser(
			final String id,
			final String email,
			final String password) {
		User making = new User(id);
		_muda.create(making, password);
		return making;
	}
	
	public List getUsers() {
		return _muda.getAllUsers();
	}
	
	public boolean existUser(final String id) {
		return _muda.exists(new User(id));
	}
	
	public User getUser(final String id) {
		return new User(id);
	}
	
	public void syncUser(final User user) {
		
	}
	
	public String getPassword(final User user) {
		return _muda.getPassword(user);
	}
	
	public void setPassword(final User user, final String password ) {
		_muda.setPassword(user, password);
	}
}
