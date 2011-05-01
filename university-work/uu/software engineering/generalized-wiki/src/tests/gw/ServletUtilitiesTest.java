package gw;

import java.util.StringTokenizer;

import junit.framework.TestCase;
import gw.ServletUtilities;
import gw.render.StylesheetCreateException;
import gw.storage.StorageException;
import gw.render.MockStorage;

public class ServletUtilitiesTest extends TestCase {

	public void testJoinPathFromArray() {
		String[] dir_array = { "foo", "bar", "toto" };
		String result = ServletUtilities.joinPathFromArray(dir_array, 2);
		assertEquals(result, "foo/bar/");
	}

	private void assertPathInfoIsNormalized(String pathinfo, boolean isDir) {
		StringTokenizer st = new StringTokenizer(pathinfo, "/", true);
		String dirPart = null;
	
		//at least one token which is the root
		assertTrue(st.hasMoreElements());
		
		//every directory part should start with a slash and 
		//it's name should not reference to current or parent dir
		while(st.hasMoreElements()) {
			dirPart = st.nextToken();
			assertTrue(dirPart.equals("/"));
			if(st.hasMoreElements()) {
				dirPart = st.nextToken();
				assertFalse(dirPart.equals("."));
				assertFalse(dirPart.equals(".."));			
				assertFalse(dirPart.equals("/"));
			} else if(!isDir) {
				fail("file should not end with a slash");
			}				
		}
		//directories should end with a slash 
		if(isDir)
			assertTrue(dirPart.equals("/"));
	}

	public void testNormalizedBigPathInfo() throws StorageException, StylesheetCreateException {
		MockStorage storage = new MockStorage();
		String pathinfo = ServletUtilities.normalizePathInfo(storage, "dir/../..///.//../foo//dir/../.", "/foo");
		assertPathInfoIsNormalized(pathinfo, true);
		assertEquals("/foo/", pathinfo);
	}

	public void testNormalizedCurrentDir() throws StorageException, StylesheetCreateException {
		MockStorage storage = new MockStorage();
		String pathinfo = ServletUtilities.normalizePathInfo(storage, ".", "/foo");
		assertPathInfoIsNormalized(pathinfo, true);
		assertEquals("/foo/", pathinfo);
	}

	public void testNormalizedEmpty() throws StorageException, StylesheetCreateException {
		MockStorage storage = new MockStorage();
		String pathinfo = ServletUtilities.normalizePathInfo(storage, "");
		assertPathInfoIsNormalized(pathinfo, true);
		assertEquals("/", pathinfo);
	}

	public void testNormalizedParentDir() throws StorageException, StylesheetCreateException {
		MockStorage storage = new MockStorage();
		String pathinfo = ServletUtilities.normalizePathInfo(storage, "..", "/foo");
		assertPathInfoIsNormalized(pathinfo, true);
		assertEquals("/", pathinfo);
	}

	public void testNormalizedRelativeDir() throws StorageException, StylesheetCreateException {
		MockStorage storage = new MockStorage();
		String pathinfo = ServletUtilities.normalizePathInfo(storage, "dir", "/bar");
		assertPathInfoIsNormalized(pathinfo, true);
		assertEquals("/bar/dir/", pathinfo);
	}

}
