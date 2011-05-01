package gw.render.test;

import org.jdom.Element;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;
import gw.GwConstants;
import gw.render.parsers.*;
import gw.render.ParserFactory;
import gw.render.ParserFactoryException;

public class ParserTest extends TestCase {
	
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(ParserTest.class);
    }

	public void testParserFactory() throws ParserFactoryException {
		ParserFactory fac = new ParserFactory();
		fac.register(GwConstants.GWML_MIME_TYPE,GwConstants.XML_MIME_TYPE, XMLParser.getInstance());
		Parser parser = fac.lookupByType(GwConstants.GWML_MIME_TYPE,GwConstants.XML_MIME_TYPE);
		assertNotNull(parser);
	}
    
	public void testDefaultParser() throws ParseException{
		Element root = (org.jdom.Element) (new DefaultParser().parse("fooBar"));
		assertEquals("root", root.getName());
		assertEquals("fooBar", root.getText());
	}
	
	public void testBibToPlainTextParser() throws ParseException{
		String output = (String) BibToPlainTextParser.getInstance().parse("@InProceedings{DV02-csmr, author = {Arie van Deursen and Eelco Visser}}");
		assertEquals("@InProceedings{DV02-csmr, author = {Arie van Deursen and Eelco Visser}}", output);
	}
	
	public void testGWMLParser() throws ParseException {
		Element root = (org.jdom.Element) (new XMLParser().parse("<root>foo</root>"));
		assertEquals("root", root.getName());
		assertEquals("foo", root.getText());
		
	}
	
	public void testGWMLNamespace() throws ParseException {
		Element root = (org.jdom.Element) (new XMLParser().parse("<root xmlns:gw=\"http://www.cs.uu.nl/wiki/Gw\"><gw:b>foo</gw:b></root>"));
		assertEquals("root", root.getName());
		Element b = ((Element) root.getChildren().get(0));
		assertEquals("b", b.getName());
		assertEquals("http://www.cs.uu.nl/wiki/Gw", b.getNamespace().getURI());
		assertEquals("foo", b.getText());
	}
}
