package gw.users;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;

import com.mockobjects.Verifiable;
import com.mockobjects.util.Verifier;

/**
 *	Mocks a user data adapter, storing any submitted users in a hash map. 
 */
public class MockUserDataAdapter implements UserDataAdapter, Verifiable {
	private HashMap _store;

	public MockUserDataAdapter() {
		_store = new HashMap();		
	}
	
	public void create(final User user, final String password ) {
		_store.put(user.getId(), password);
	}
	
	public void setPassword( final User user, final String password ){
		if(_store.remove(user.getId()) == null)
			return;
		_store.put(user.getId(), password);
	}
	
	public String getPassword( final User user ) {
		return (String) _store.get(user.getId());
	}
	
	public void delete(final User user) throws NoSuchUserException {
		if(_store.remove(user.getId()) == null)
			throw new NoSuchUserException("");
	}
	
	public void load(final User user) throws NoSuchUserException {
		if(_store.get(user.getId()) == null)
			throw new NoSuchUserException("");
	}
	
	public void store(final User user) {
		if(_store.get(user.getId()) == null)
			create(user, null);
	}
	
	public List getAllUsers() {
		ArrayList al = new ArrayList();
		al.addAll(_store.keySet());
		return al;
	}
	
	public boolean exists( final User user ) {
		if(_store.get(user.getId()) == null)
			return false;
		return true;
	}
	
	public void verify(){ 
		Verifier.verifyObject(this); 
	} 
}
