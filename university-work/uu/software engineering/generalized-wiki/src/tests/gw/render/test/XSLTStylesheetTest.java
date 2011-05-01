package gw.render.test;

import gw.GwContext;
import gw.GwSessionContext;
import gw.render.XSLTStylesheet;
import gw.render.MyServletContext;

import java.io.ByteArrayInputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

import org.jdom.Element;
import org.jdom.transform.JDOMSource;

import junit.framework.TestCase;

public class XSLTStylesheetTest extends TestCase {
	
	private GwContext context = null;
	private GwSessionContext sessionContext = null;
	private String styleInput = "";
	
    public XSLTStylesheetTest(final String name) {
        super(name);
    }
    
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(XSLTStylesheetTest.class);
    }
	
    protected void setUp() throws Exception {
    	context = new GwContext(new MyServletContext());
    	sessionContext = new GwSessionContext(context);
    	
		styleInput = "";
		styleInput += "<?xml version=\"1.0\"?>";
		styleInput += "<xsl:stylesheet version=\"1.0\"";
		styleInput +="  xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\"";
		styleInput +="  xmlns:gw=\"http://www.cs.uu.nl/wiki/Gw\"";
		styleInput += "  xmlns:xhtml=\"http:/www.w3.org/1999/xhtml\"";
		styleInput +="   xmlns=\"http://www.w3.org/1999/xhtml\">";
		styleInput +="         <html xmlns=\"http:/www.w3.org/1999/xhtml\">";
		styleInput += "        <head>";
		styleInput += "          <title>";
		styleInput += "				test";
		styleInput += "          </title>";
		styleInput += "        </head>";
		styleInput += "	        <body>";
		styleInput += "				test";
		styleInput += "          </body>";
		styleInput += "        </html>";
		styleInput += 	"</xsl:stylesheet>";
	}

	protected void tearDown() throws Exception {
		context = null;
		sessionContext = null;
	}
	
	public void testCreate()
	{
		try
		{
			byte styleBytes[] = styleInput.getBytes();
			ByteArrayInputStream styleInputStream = new ByteArrayInputStream(styleBytes);
			XSLTStylesheet sheet = new XSLTStylesheet(styleInputStream, sessionContext, context);
			styleInputStream.close();
			assertNotNull(sheet);
		}
		catch(Exception e)
		{ 
			e.printStackTrace();
			fail("could not create XSLTStylesheet: " + e.toString());
		}
	}
	public void testGetMimeType()
	{
		try
		{
			byte styleBytes[] = styleInput.getBytes();
			ByteArrayInputStream styleInputStream = new ByteArrayInputStream(styleBytes);
			XSLTStylesheet sheet = new XSLTStylesheet(styleInputStream, sessionContext, context);
			styleInputStream.close();
			String mimetype = sheet.getMimeType(null);
			assertNull(mimetype);
		}
		catch(Exception e)
		{ 
			e.printStackTrace();
			fail("could not getMimeType() from XSLTStylesheet: " + e.toString());
		}
		
		assertTrue(true);
	}
	
	public void testApply()
	{
		try
		{
			Element root = new Element("test");
			JDOMSource source = new JDOMSource(root);	
			PipedInputStream pis = new PipedInputStream();
			PipedOutputStream pos = new PipedOutputStream();
			pos.connect(pis);
			Result result = new StreamResult(pos);
		
			byte styleBytes[] = styleInput.getBytes();
			ByteArrayInputStream styleInputStream = new ByteArrayInputStream(styleBytes);
			XSLTStylesheet sheet = new XSLTStylesheet(styleInputStream, sessionContext, context);
			
			sheet.apply(source, "",  result);
			pos.close();
			pis.close();
		}
		catch(Exception e)
		{ 
			e.printStackTrace();
			fail("could not apply() for XSLTStylesheet: " + e.toString());
		}	
		
		assertTrue(true);
	}
}
