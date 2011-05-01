package gw.render.parsers.test;

import gw.render.parsers.BibQueryParser;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.*;
import org.jdom.Document;
import org.jdom.Element;

public class BibQueryParserTest extends TestCase {

	BibQueryParser _bibQueryParser;

	/**
	 * Construct a new Testcase with the given name
	 * 
	 * @param name
	 */
	public BibQueryParserTest(final String name) {
		super(name);
	}

	public void testGetBibtexQuery() {
		Document doc = new Document();
		Element el = new Element ("document");
		Element el2 = new Element ("query");
		el2.addContent("this is a query");
		el.addContent(el2);
		doc.addContent(el);
		assertEquals(_bibQueryParser.getBibtexQuery( doc ),"this is a query");
	}
	
	public void testGetSortingOrder() {
		Document doc = new Document();
		Element el = new Element ("document");
		Element el2 = new Element ("sort");
		el2.addContent("year");
		el.addContent(el2);
		doc.addContent(el);
		assertEquals(_bibQueryParser.getBibtexSortingOrder(doc).getText(),"year");
	}

	//Throw exeption if no include statement is found.
	public void testNoIncludeStatement() {
		String input = "<document></document>";
		
		try {
			_bibQueryParser.parse(input);
			fail("No include statement found");
		}
		catch(Exception e) {
			assertTrue(true);
		}
	}

	/**
	 * Initialize for each test
	 */
	protected void setUp() {
		_bibQueryParser = new BibQueryParser();
	}

	/**
	 * Destruct for each test
	 */
	protected void tearDown() {
		_bibQueryParser = null;
	}

	/**
	 * Create a TestSuite based on this TestCase
	 * 
	 * @return the generated TestSuite
	 */
	public static Test suite() {
		TestSuite suite = new TestSuite(BibQueryParserTest.class);
		return suite;
	}

}
