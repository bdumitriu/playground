package gw.users;

import gw.storage.Storage;

public class MockAuthenticationManagerFactory implements AuthenticationManagerFactory {
	
	public MockAuthenticationManagerFactory() {		
	}

	public AuthenticationManager getAuthenticationManager(Storage unsecuredStorage) {
		return new MockAuthenticationManager();
	}
}
