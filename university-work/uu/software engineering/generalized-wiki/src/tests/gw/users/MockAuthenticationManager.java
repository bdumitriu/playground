package gw.users;

import java.util.*;

import com.mockobjects.Verifiable;
import com.mockobjects.util.Verifier;


/**
 * A mock object used for testing purposes.
 */
public class MockAuthenticationManager implements AuthenticationManager, Verifiable {
    private String _currentUserId;
    
    public MockAuthenticationManager() {
    	
    }

	public boolean login(String userId, Map authParameters) {
		_currentUserId = userId;
        return true;
	}

	public void logout() {
		_currentUserId = null;
	}
	
	public void verify(){ 
		Verifier.verifyObject(this); 
	} 
}
