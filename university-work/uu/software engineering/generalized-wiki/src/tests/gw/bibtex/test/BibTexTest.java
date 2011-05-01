package gw.bibtex.test;

import gw.util.BibTex;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class BibTexTest extends TestCase {

	BibTex _bibtex;

	/**
	 * Construct a new Testcase with the given name
	 * 
	 * @param name
	 */
	public BibTexTest(final String name) {
		super(name);
	}

	public void testProcessSortunder() {

		assertEquals(BibTex
				.processSortunder("{\\sortunder{Beek} Prof. Dr. van Beek}"),
				"Beek{\\sortunder{Beek} Prof. Dr. van Beek}");
		assertEquals(BibTex.processSortunder("Arie Beek"), "Arie Beek");
	}

	/**
	 * Initialize for each test
	 */
	protected void setUp() {
		_bibtex = new BibTex();
	}

	/**
	 * Destruct for each test
	 */
	protected void tearDown() {
		_bibtex = null;
	}

	/**
	 * Create a TestSuite based on this TestCase
	 * 
	 * @return the generated TestSuite
	 */
	public static Test suite() {
		TestSuite suite = new TestSuite(BibTexTest.class);
		return suite;
	}

}
