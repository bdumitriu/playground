package gw.render.test;

import static gw.GwConstants.GWML_MIME_TYPE;

import gw.GwContext;
import gw.GwSessionContext;
import gw.render.RegisterException;
import gw.render.Stylesheet;
import gw.render.StylesheetApplyException;
import gw.render.StylesheetFactory;
import gw.render.IdentityStylesheet;
import gw.render.MyServletContext;

import javax.xml.transform.Result;
import javax.xml.transform.Source;

import junit.framework.TestCase;

public class StylesheetFactoryTest extends TestCase {
	
	private GwContext context = null;
	private GwSessionContext sessionContext = null;
	
    public StylesheetFactoryTest(final String name) {
        super(name);
    }
    
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(StylesheetFactoryTest.class);
    }
	
    protected void setUp() throws Exception {
    	context = new GwContext(new MyServletContext());
    	sessionContext = new GwSessionContext(context);
	}

	protected void tearDown() throws Exception {
		context = null;
		sessionContext = null;
	}
	
    public void testRegisterNewInstance()
    {
    	try
    	{
	        Stylesheet stylesheet;
	        StylesheetFactory factory = new StylesheetFactory();
	        try{ factory.register(GWML_MIME_TYPE, WrongStylesheet.class); }
	        catch (RegisterException exc){ /* expected to be thrown */}
	        stylesheet = factory.newByType(GWML_MIME_TYPE, null, context, sessionContext);
	        assertNotNull(stylesheet);
    	}
    	catch(Exception e){ e.printStackTrace(); }
    }
    
    public void testRegisterWrongConstructor()
    {
        String key = GWML_MIME_TYPE;
        StylesheetFactory factory = new StylesheetFactory();
        
        try {
            factory.register(key, WrongStylesheet.class);
            fail("registraton of a class without a Constructor(InputStream)");
        } catch (RegisterException e) {
            assertTrue(true);
        }
    }
    
    public void testIdentityStyleSheet()
    {
    	StylesheetFactory factory = new StylesheetFactory();
    	if (factory.newIdentity(context, sessionContext) == null)
    		fail("StylesheetFactory.identityStyleSheet() gives null");
    	else
    		assertTrue(true);
    }
    
    public void testNewByType()
    {
    	StylesheetFactory factory = new StylesheetFactory();
    	try
    	{
    		Stylesheet style = factory.newByType("", null, context, sessionContext);
  
	    	if(style == null)
	    		fail("StylesheetFactory.newByType() gives null");
	    	
	    	if(!(style instanceof IdentityStylesheet))
	    		fail("Unexpected stylesheet returned when invoking: StylesheetFactory.newByType()");
    	}
    	catch(Exception e)
    	{ 
    		e.printStackTrace(); 
    		fail("StylesheetFactory.newByType() trhows exception: " + e.toString()); 
    	}
    	
    	assertTrue(true);
    }

    class WrongStylesheet implements Stylesheet {
        public void apply(Source source, String pathinfo, Result result)
        	throws StylesheetApplyException {
        }
        
        public String getMimeType(String pathinfo){
        	return "";
        }
    }
}
