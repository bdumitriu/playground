package gw.users;

import junit.framework.*;

public class DataAdapterUserManagerFactoryTest extends TestCase {
	private DataAdapterUserManagerFactory _daumf;
	
	public DataAdapterUserManagerFactoryTest(String name) {
		super(name);
	}
	
	protected void setUp() {
		_daumf = new DataAdapterUserManagerFactory(new MockUserDataAdapter());
	}
	
	protected void tearDown() {
		_daumf = null;
	}
	
	public void testDelivery() {
		assertTrue(_daumf.getUserManager() instanceof UserManager);
	}
}
