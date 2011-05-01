package gw.users;

import gw.GwContext;
import gw.GwSessionContext;
import gw.MockServletContext;
import gw.storage.MockStorage;

import junit.framework.*;

public class UserDataAdapterFactoryTest extends TestCase {
	MockServletContext _msc = new MockServletContext();
	GwContext _gc = new GwContext(_msc);
	GwSessionContext _gsc = new GwSessionContext(_gc);
	
	public UserDataAdapterFactoryTest(String name) {
		super(name);
	}
	
	public void testDelivery() {
		assertTrue(UserDataAdapterFactory.getUserDataAdapter(_gsc.getUnsecuredStorage()) instanceof UserDataAdapter);
	}
}
