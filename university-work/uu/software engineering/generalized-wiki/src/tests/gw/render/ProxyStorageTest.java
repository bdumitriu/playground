package gw.render;

import gw.render.ProxyStorage;
import gw.storage.StorageException;

import java.util.Hashtable;
import java.util.Map;

import java.io.ByteArrayInputStream;

import junit.framework.TestCase;

public class ProxyStorageTest extends TestCase {

	public void testVirtualFile() throws StorageException {
		MockStorage storage = new MockStorage();
		Map<String, String> properties = new Hashtable<String, String>();
		ByteArrayInputStream data = new ByteArrayInputStream("".getBytes());
        ProxyStorage proxyStorage = new ProxyStorage(storage);
        proxyStorage.addVirtualFile("/virtualfile", data, properties);

		assertTrue(proxyStorage.fileExists("/virtualfile"));
		assertSame("Proxy should return virtual properties", properties, proxyStorage.getProperties("/virtualfile"));
		assertSame("Proxy should return virtual input", data, proxyStorage.getFile("/virtualfile"));
	}
	
    //TODO make a lot more tests - and better tests
	public void testNoVirtualFile() throws StorageException {
		MockStorage storage = new MockStorage();
		Map<String, String> properties = new Hashtable<String, String>();
		ByteArrayInputStream data = new ByteArrayInputStream("".getBytes());
        ProxyStorage proxyStorage = new ProxyStorage(storage);
        proxyStorage.addVirtualFile("/virtualfile", data, properties);

		assertNotSame("Proxy should not return virtual properties", properties, proxyStorage.getProperties("/unknownfile"));
		assertNotSame("Proxy should not return virtual input", data, proxyStorage.getFile("/unknownfile"));
	}
}
