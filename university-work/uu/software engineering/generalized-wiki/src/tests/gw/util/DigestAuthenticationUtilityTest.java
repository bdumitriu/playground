package gw.util;

import gw.util.DigestAuthenticationUtility;
import junit.framework.*;
/**
 * Test for the DigestAuthenticationUtility class.
 */
public class DigestAuthenticationUtilityTest extends TestCase {

    public DigestAuthenticationUtilityTest( final String fixture ) {

        super(fixture);
    }

    /**
     * Tests if the hashes match. If they do, the hash facility should
     * be correct. The first hash comes directly from an RFC.
     * 
     * Note: these hashes are dependend on the REALM.
     */
    public void testHashing()
    {
        String passwordHash = DigestAuthenticationUtility.getPasswordHash("Mufasa", "Circle Of Life");
        assertEquals(passwordHash, "8dfe9db29933de5ceec79512f1381500");

        String resultHash = DigestAuthenticationUtility.getResultHash(passwordHash, "dcd98b7102dd2f0e8b11d0f600bfb0c093", "00000001", "0a4f113b", "auth", "GET", "/dir/index.html");
        assertEquals(resultHash, "a6d87023c2277d8358974a90afa42d07");
    }

    /**
     * Run this testcase in textmode
     * @param args
     */
    public static void main( String[] args ) {

      junit.textui.TestRunner.run(suite());
    }

    /**
     * Create a TestSuite based on this TestCase
     * @return the generated TestSuite
     */
    public static Test suite() {

      TestSuite suite = new TestSuite(DigestAuthenticationUtilityTest.class);
      return suite;
    }
}
