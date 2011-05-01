package gw.blog.test;

import gw.GwConstants;
import gw.blog.PreviewComment;
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

public class PreviewCommentTest extends TestCase {
    
    private String pathInfo;
    private PreviewComment previewComment;
    private XMLFactory factory;
    private MockGwContext context;
    private ProxyStorage proxyStorage;
    private MockHttpServletRequest request;
    
    public PreviewCommentTest(final String name) {
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
        TestSuite suite = new TestSuite(PreviewCommentTest.class);
        return suite;
    }
    
    protected void setUp() throws XMLFactoryException, StorageException {
    	previewComment = new PreviewComment();
    	proxyStorage = new ProxyStorage(new MockBlogStorage());
    	
    	factory = new XMLFactory("/testPath/comment.xml",proxyStorage);
        context = new MockGwContext();
        context.registerParser(GwConstants.GWML_MIME_TYPE, GwConstants.XML_MIME_TYPE, new MockParser());

        proxyStorage = new ProxyStorage(new MockBlogStorage());
        
        pathInfo = "blog/testblog/comment";
        
        request = new MockHttpServletRequest();
        request.setupPathInfo("blog/testblog/comment");
        request.setupAddParameter("entry", "Test Comment");
        request.setupAddParameter("contenttype", GwConstants.GWML_MIME_TYPE);
        
    }
    
    /**
     * Test the StoreComment() function
     */
    public void testStoreComment() throws XMLFactoryException, StorageException {
        previewComment.storeComment(context, proxyStorage, request, "test_user");

        InputStream in = proxyStorage.getFile(pathInfo);
        assertNotNull("Virtual file must exist", in);
        
        Map properties = proxyStorage.getProperties(pathInfo);
        String contentType = (String) properties.get("content-type");
        assertEquals("ContentType must be blog/comment", contentType, GwConstants.BLOG_COMMENT_MIME_TYPE);
    }
    
    /**
     * Test the storeCommentIntoFactory() function
     */
    public void teststoreCommentIntoFactory() throws XMLFactoryException {
        previewComment.storeCommentIntoFactory(context, factory, request, "testUserName");
            
        Element root = factory.getRoot();
        assertNotNull(root);
        
        assertEquals(factory.getFirstElementChildByName("content",root).getNodeName(),("content"));
        assertEquals(factory.getFirstElementChildByName("author",root).getNodeName(),("author"));
        assertEquals(factory.getFirstTextNodeChild(factory.getFirstElementChildByName("author",root)).getNodeValue(),("testUserName"));     
    }
    
    
}
