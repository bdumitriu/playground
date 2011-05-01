package gw.bibtex.test;

import junit.framework.TestCase;
import gw.util.RegExp;

public class RegExpTest extends TestCase {

	public RegExpTest(String arg0) {
		super(arg0);
	}

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/*
	 * Test method for 'gw.util.RegExp.replace(String, String, String)'
	 */
	public void testReplace() {
		assertEquals(RegExp.replace("Generalizing Wiki", "i", "I"),
				"GeneralIzIng WIkI");
	}

	/*
	 * Test method for 'gw.util.RegExp.returnMatchedGroup(String, String, int)'
	 */
	public void testReturnMatchedGroup() {

		assertEquals(RegExp.returnMatchedGroup("testing matching procedure",
				"testing (.*) procedure", 1), "matching");

		assertEquals(RegExp.returnMatchedGroup(
				"{\\sortunder{Beek} Prof. Dr. van Beek van Doornenbos}et al",
				"\\{\\\\sortunder\\{(.*)\\}(.*)\\}(.*)", 2),
				" Prof. Dr. van Beek van Doornenbos");
	}

}
