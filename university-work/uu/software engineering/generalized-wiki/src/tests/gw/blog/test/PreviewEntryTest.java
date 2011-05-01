package gw.blog.test;

import gw.GwConstants;
import gw.blog.PreviewEntry;
import gw.blog.XMLFactory;
import gw.blog.XMLFactoryException;
import gw.render.ProxyStorage;
import gw.storage.StorageException;

import java.io.InputStream;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.w3c.dom.Element;

import com.mockobjects.servlet.MockHttpServletRequest;

public class PreviewEntryTest extends TestCase{
    
    private String pathInfo;
    private PreviewEntry editEntry;
    private XMLFactory factory;
    private MockGwContext context;
    private ProxyStorage proxyStorage;
    private MockHttpServletRequest request;
    
    public PreviewEntryTest(final String name) {
        super(name);
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
        TestSuite suite = new TestSuite(PreviewEntryTest.class);
        return suite;
    }

    /**
     * Initialize for each test
     * @throws StorageException 
     */ 
    protected void setUp() throws XMLFactoryException, StorageException {
    	editEntry = new PreviewEntry();
    	context = new MockGwContext();
        editEntry = new PreviewEntry();
        
        context.registerParser(GwConstants.GWML_MIME_TYPE, GwConstants.XML_MIME_TYPE, new MockParser());

        proxyStorage = new ProxyStorage(new MockBlogStorage());
        factory = new XMLFactory("/testPath/entry.xml",proxyStorage);
        pathInfo = "blog/testblog/entry";
        
        request = new MockHttpServletRequest();
        request.setupPathInfo(pathInfo);
        request.setupAddParameter("title", "Test Title");
        request.setupAddParameter("entry", "Test Entry");
        request.setupAddParameter("contenttype", GwConstants.GWML_MIME_TYPE);
        request.setupAddParameter("draft", "no");
        request.setupAddParameter("draft", "no");
    }
 
    /**
     * Test the testStoreEntry function
     */
    public void testStoreEntry() throws XMLFactoryException, StorageException {
        editEntry.storeEntry(context, proxyStorage, request, "test_user");

        InputStream in = proxyStorage.getFile(pathInfo);
        assertNotNull("Virtual file must exist", in);
        
        Map properties = proxyStorage.getProperties(pathInfo);
        String contentType = (String) properties.get("content-type");
        assertEquals("ContentType must be blog/entry", contentType, GwConstants.BLOG_ENTRY_MIME_TYPE);        
    }
    
    /**
     * Test the storeEntryInFactory function
     */
    public void teststoreEntryInFactory() throws XMLFactoryException
    {
        editEntry.storeEntryInFactory(context, factory, request, "testUserName");
        
        Element root = factory.getRoot();
        assertNotNull(root);
        assertEquals(factory.getFirstElementChildByName("content",root).getNodeName(),("content"));
        assertEquals(factory.getFirstElementChildByName("title",root).getNodeName(),("title"));
        assertEquals(factory.getFirstElementChildByName("date",root).getNodeName(),("date"));
        assertEquals(factory.getFirstElementChildByName("author",root).getNodeName(),("author"));
        assertEquals(factory.getFirstTextNodeChild(factory.getFirstElementChildByName("title",root)).getNodeValue(),("Test Title"));
        assertEquals(factory.getFirstTextNodeChild(factory.getFirstElementChildByName("author",root)).getNodeValue(),("testUserName"));
    }
    
}
