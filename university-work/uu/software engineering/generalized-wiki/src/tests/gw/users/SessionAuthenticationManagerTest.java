package gw.users;

import gw.GwContext;
import gw.GwSessionContext;
import gw.MockServletContext;

import java.util.HashMap;
import junit.framework.*;

public class SessionAuthenticationManagerTest extends TestCase {
	private SessionAuthenticationManager _sam;
	private MockUserDataAdapter _muda;
	
	private GwContext _gc;
	private GwSessionContext _gsc;
	private MockServletContext _msc;
	
	public SessionAuthenticationManagerTest(String name) {
		super(name);
	}
	
	protected void setUp() {
		_muda = new MockUserDataAdapter();
		_msc = new MockServletContext();
		_gc = new GwContext(_msc);
		_gsc = new GwSessionContext(_gc);
		_sam = new SessionAuthenticationManager(_gsc , _muda);
	}
	
	protected void tearDown() {
		_muda = null;
		_msc = null;
		_gc = null;
		_gsc = null;
		_sam = null;
	}
	
	public void testLoginBad() {
		User testUser = new User("Alice");
		_muda.create(testUser, "iLoveBob");
		
        HashMap authParams = new HashMap();
        authParams.put("username", "Alice");
        authParams.put("qop", "auth");
        authParams.put("nonce", "");
        authParams.put("nc", "");
        authParams.put("cnonce", "");
        authParams.put("uri", "");
        authParams.put("algorithm", "MD5");
        authParams.put("requestMethod", "");
        
        authParams.put("response", "");
        
        _sam.setNonce("");

        assertFalse(_sam.login("Alice", authParams));		
	}
	
	public void testLoginGood() {
		User testUser = new User("Alice");
		_muda.create(testUser, "iLoveBob");
		
        HashMap authParams = new HashMap();
        authParams.put("username", "Alice");
        authParams.put("qop", "auth");
        authParams.put("nonce", "lorem");
        authParams.put("nc", "ipsum");
        authParams.put("cnonce", "lorem");
        authParams.put("uri", "http://lorem.ipsum");
        authParams.put("algorithm", "MD5");
        authParams.put("requestMethod", "ipsum");
        
        authParams.put("response", "4bf639ae23ca028c87a86d051b4315ab");
        
        _sam.setNonce((String) authParams.get("nonce"));
        
        assertTrue(_sam.login(testUser.getId(), authParams));
	}
	
	public void testLogout() {
		User testUser = new User("JohnDoe");
		_gsc.setOwner(testUser);
		_sam.logout();
		assertFalse(_gsc.getOwner().equals(testUser));
	}
}
