package gw.blog.test;

import gw.GwConstants;
import gw.blog.PreviewBlog;
import gw.blog.XMLFactoryException;
import gw.render.ProxyStorage;
import gw.storage.StorageException;

import java.io.InputStream;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.jdom.Element;

import com.mockobjects.servlet.MockHttpServletRequest;

public class PreviewBlogTest extends TestCase {
    
    private String pathInfo;
    private PreviewBlog previewBlog;
    private MockHttpServletRequest request;
    private ProxyStorage proxyStorage;
    
    public PreviewBlogTest(String name) {
        super(name);
    }
    
    /**
     * Run this testcase in textmode
     * @param args
     */
    public static void main(String[] args) {
        junit.textui.TestRunner.run(suite());
    }
    
    /**
     * Create a TestSuite based on this TestCase
     * @return the generated TestSuite
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(PreviewBlogTest.class);
        return suite;
    }
    
    protected void setUp() throws XMLFactoryException {
        pathInfo = "blog/testblog/index";
        previewBlog = new PreviewBlog();
        request = new MockHttpServletRequest();
        request.setupPathInfo(pathInfo);
        request.setupAddParameter("title", "Test Title");
        request.setupAddParameter("description", "Test Description");
        proxyStorage = new ProxyStorage(new MockBlogStorage());
    }
    
    public void testCreateVirtualFile() throws XMLFactoryException, StorageException {
        previewBlog.createVirtualFile(request, proxyStorage, "jan");
        InputStream in = proxyStorage.getFile(pathInfo);
        assertNotNull("Virtual file must exist", in);

        Map properties = proxyStorage.getProperties(pathInfo);
        String contentType = (String) properties.get("content-type");
        assertEquals("ContentType must be blog/index", contentType, GwConstants.BLOG_INDEX_MIME_TYPE);
    }
    
    
    public void testProcessForm() {
        Element result = previewBlog.processForm(request);
        
        assertEquals("Name of root element must be blog", result.getName(), "blog");
        
        Element title = result.getChild("title");
        assertNotNull("Title element must exist", title);
        assertEquals("Title text must be 'Test Title'", title.getText(), "Test Title");
        
        Element description = result.getChild("description");
        assertNotNull("Description element must exist", description);
        assertEquals("Description text must be 'Test Description'", description.getText(), "Test Description");
    }
}
