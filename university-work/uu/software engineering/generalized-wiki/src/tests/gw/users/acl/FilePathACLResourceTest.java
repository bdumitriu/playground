package gw.users.acl;


import junit.framework.*;


/**
 * Tests if the permission and actions classes are actually working.
 */
public class FilePathACLResourceTest extends TestCase {
    public FilePathACLResourceTest(final String name) {
        super(name);
    }
    
    /**
     * This looks like something trivial, but apperently it is not.
     */
    public void testParentResourceWithTrailingSlash()  {
        FilePathACLResource resource = new FilePathACLResource("/a/b/c/");
        assertTrue(resource.getParentResource().getIdentifier().equals("/a/b"));
        assertTrue(resource.getParentResource().getParentResource().getIdentifier().equals("/a"));
        assertTrue(resource.getParentResource().getParentResource().getParentResource().getIdentifier().equals("/"));
        assertTrue(resource.getParentResource().getParentResource().getParentResource().getParentResource() == null);
        
        resource = new FilePathACLResource("/a/b/c");
        assertTrue(resource.getParentResource().getIdentifier().equals("/a/b"));
        assertTrue(resource.getParentResource().getParentResource().getIdentifier().equals("/a"));
        assertTrue(resource.getParentResource().getParentResource().getParentResource().getIdentifier().equals("/"));
        assertTrue(resource.getParentResource().getParentResource().getParentResource().getParentResource() == null);
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
        TestSuite suite = new TestSuite(FilePathACLResourceTest.class);
        return suite;
    }
}
