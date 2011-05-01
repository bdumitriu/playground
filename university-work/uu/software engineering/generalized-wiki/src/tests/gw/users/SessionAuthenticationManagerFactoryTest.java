package gw.users;

import gw.storage.MockStorage;
import gw.GwContext;
import gw.GwSessionContext;
import gw.MockServletContext;
import junit.framework.*;

public class SessionAuthenticationManagerFactoryTest extends TestCase {
	MockServletContext _msc = new MockServletContext();
	GwContext _gc = new GwContext(_msc);
	GwSessionContext _gsc = new GwSessionContext(_gc);
	SessionAuthenticationManagerFactory _samf;
	
	public SessionAuthenticationManagerFactoryTest(String name) {
		super(name);
	}
	
	protected void setUp() {
		_samf = new SessionAuthenticationManagerFactory(_gsc);
	}
	
	protected void tearDown() {
		_samf = null;
	}
	
	public void testDelivery() {
		assertTrue(_samf.getAuthenticationManager(_gsc.getUnsecuredStorage()) instanceof AuthenticationManager);
	}
}
