package gw.render.test;

import gw.GwContext;
import gw.GwSessionContext;
import gw.render.MyServletContext;
import gw.render.TransformationResource;
import gw.render.IdentityStylesheet;
import gw.render.Stylesheet;

import javax.xml.transform.Source;

import org.jdom.Element;
import org.jdom.transform.JDOMSource;

import junit.framework.TestCase;

public class TransformationResourceTest extends TestCase {
	
    public TransformationResourceTest(final String name) {
        super(name);
    }
    
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(TransformationResourceTest.class);
    }

    public void testCreateNull()
    {	
    	TransformationResource tr = new TransformationResource(null, null);
    	assertNull(tr.getStylesheet());
    	assertNull(tr.getSource());
    }
    
    public void testCreateWithFstNull()
    {	
    	Stylesheet s = new IdentityStylesheet(new GwSessionContext(new GwContext(new MyServletContext())));
    	TransformationResource tr = new TransformationResource(s, null);
    	assertEquals(tr.getStylesheet(), s);
    	assertNull(tr.getSource());
    }
    
    public void testCreateWithSndNull()
    {	
		Source source = new JDOMSource(new Element("test-elem"));	    				
		
    	TransformationResource tr = new TransformationResource(null, source);
    	assertEquals(tr.getSource(), source);
    	assertNull(tr.getStylesheet());
    }
    
    public void testCreateFull()
    {	
		Source source = new JDOMSource(new Element("test-elem"));	    				
    	Stylesheet s = new IdentityStylesheet(new GwSessionContext(new GwContext(new MyServletContext())));
    	TransformationResource tr = new TransformationResource(s, source);
    	
    	assertEquals(tr.getStylesheet(), s);
    	assertEquals(tr.getSource(), source);
    }
    
    public void testSetStylesheet()
    {
		Source source = new JDOMSource(new Element("test-elem"));	    				
		
    	TransformationResource tr = new TransformationResource(null, source);
    	assertNull(tr.getStylesheet());
    	
    	Stylesheet s = new IdentityStylesheet(new GwSessionContext(new GwContext(new MyServletContext())));
    	tr.setStylesheet(s);
    	assertEquals(tr.getSource(), source);
    	assertEquals(tr.getStylesheet(), s);
    }
}
