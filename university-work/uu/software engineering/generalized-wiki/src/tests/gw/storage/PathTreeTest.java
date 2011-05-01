package gw.storage;

import gw.storage.javasvn.CommitItem;
import gw.storage.javasvn.PathTree;
import gw.storage.javasvn.SVNRACheckoutEditor;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Test cases for checking the methods of the PathTree class.
 *
 * @author Bogdan Dumitriu
 */
public class PathTreeTest extends TestCase
{
    private PathTree<String> _pTree;
    private SVNRepository _repository;

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        DAVRepositoryFactory.setup();
        SVNRepositoryFactoryImpl.setup();
        _repository = SVNRepositoryFactory.create(SVNURL.parseURIEncoded("svn://localhost:10000/"));
        ISVNAuthenticationManager authManager =
            SVNWCUtil.createDefaultAuthenticationManager("gwuser", "gwpassword");
        _repository.setAuthenticationManager(authManager);
    }

    public void testEmpty() throws Exception
    {}

/*
    public void testCreate() throws Exception
    {
        SVNRACheckoutEditor editor = new SVNRACheckoutEditor();
        _repository.checkout(-1, null, true, editor);

        _pTree = new PathTree<String>(editor.getPaths(), "");
        //printSortedTree("");
        _pTree.registerMkdir("branches/dirListing/dir3/dir34/dir341");
        _pTree.registerStoreFile("branches/dirListing/dir3/dir34/dir341/dir3411/file34111", null);
        _pTree.registerDeletePath("/branches/dirListing/dir3/dir34/dir341/dir3411/file34111", true);
        _pTree.registerDeletePath("/branches/dirListing", true);
        _pTree.registerStoreFile("branches/dirListing/dir3/dir34/dir341/dir3411/file34111", null);
        printSortedTree("branches/dirListing");
        Collection<CommitItem> cis = _pTree.getCommitItems();
        for (CommitItem ci : cis)
        {
            System.out.println(ci);
        }
    }

    public void printSortedTree(String basePath)
    {
        Map<String, String> values = _pTree.getNodeValues("", false, true);
        String[] sortedPaths = values.keySet().toArray(new String[0]);
        Arrays.sort(sortedPaths);
        
        for (String path : sortedPaths)
        {
            if (path.startsWith(basePath))
            {
                System.out.println(path);
            }
        }
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        HashMap<String, String> map = new HashMap<String, String>();
        map.put("trunk", "1");
        map.put("trunk/src", "2");
        map.put("trunk/lib", "3");
        map.put("trunk/src/Main.java", "4");
        map.put("trunk/src/server/Server.java", "5");
        map.put("trunk/src/server", "6");
        map.put("trunk/lib/rt.jar", "7");
        map.put("branches/commit/some-dir/some-file", "8");
        map.put("trunk/src/config.prop", "9");

        _pTree = new PathTree<String>(map, "/base1/base2/", null);
    }

    public void testCreate() throws Exception
    {
        assertTrue(_pTree.getNodeValue("trunk").equals("1"));
        assertTrue(_pTree.hasNode("trunk"));
        assertEquals(_pTree.getNodeValue("trunk/src"), "2");
        assertEquals(_pTree.getNodeValue("trunk/lib"), "3");
        assertEquals(_pTree.getNodeValue("trunk/src/Main.java"), "4");
        assertEquals(_pTree.getNodeValue("trunk/src/server/Server.java"), "5");
        assertTrue(_pTree.hasNode("trunk/src/server/Server.java"));
        assertEquals(_pTree.getNodeValue("trunk/src/server"), "6");
        assertEquals(_pTree.getNodeValue("trunk/lib/rt.jar"), "7");
        assertNull(_pTree.getNodeValue("branches"));
        assertNull(_pTree.getNodeValue("branches/commit"));
        assertNull(_pTree.getNodeValue("branches/commit/some-dir"));
        assertEquals(_pTree.getNodeValue("branches/commit/some-dir/some-file"), "8");
        assertEquals(_pTree.getNodeValue("trunk/src/config.prop"), "9");
        assertNull(_pTree.getNodeValue("trunk/src/config.props"));
        assertFalse(_pTree.hasNode("trunk/src/config.props"));
        
        assertEquals(_pTree.getParentValue("trunk/src/server/Server.java"), "6");
        assertEquals(_pTree.getParentValue("trunk/src/server"), "2");
        assertEquals(_pTree.getParentValue("trunk/src"), "1");
        assertNull(_pTree.getParentValue("trunk/src/bla"));
        assertNull(_pTree.getParentValue("branches/commit"));
    }

    public void testInsert() throws Exception
    {
        _pTree.insertNode("trunk/src/conf/config.prop", "10");
        assertEquals(_pTree.getNodeValue("trunk/src/conf/config.prop"), "10");
        assertNull(_pTree.getNodeValue("trunk/src/conf"));
        assertTrue(_pTree.hasNode("trunk/src/conf"));
        assertTrue(_pTree.setNodeValue("trunk/src/conf", "11"));
        assertTrue(_pTree.hasNode("trunk/src/conf/config.prop"));
        _pTree.insertNode("trunk/src/conf", "12");
        assertEquals(_pTree.getNodeValue("trunk/src/conf"), "12");
        assertFalse(_pTree.hasNode("trunk/src/conf/config.prop"));
        assertFalse(_pTree.setNodeValue("trunk/src/conf/config.prop", ""));
    }

    public void testDelete() throws Exception
    {
        assertNull(_pTree.deleteNode("tags"));
        assertNull(_pTree.deleteNode("trunk/src/server/Client.java"));
        assertEquals(_pTree.deleteNode("trunk/src/server/Server.java"), "5");
        assertNull(_pTree.getNodeValue("trunk/src/server/Server.java"));
        assertEquals(_pTree.getNodeValue("trunk/src/server/"), "6");
        assertNull(_pTree.deleteNode("branches/commit"));
        assertNull(_pTree.getNodeValue("branches/commit/some-dir/some-file"));
        assertNull(_pTree.deleteNode("branches"));
        assertEquals(_pTree.deleteNode("trunk"), "1");
        assertNull(_pTree.getNodeValue("trunnk/lib"));
        assertNull(_pTree.deleteNode(""));
    }

    public void testRunItem() throws Exception
    {
        CommitItem item;

        // add directory
        item = new CommitItem(true, "/base1/base2/branches/commit/src", true);
        _pTree.runItem(item, "i1");
        assertEquals(_pTree.getNodeValue("/branches/commit/src/"), "i1");

        // add directory to another path
        item = new CommitItem(true, "branches/commit/bin", true);
        _pTree.runItem(item, null);
        assertFalse(_pTree.hasNode("branches/commit/bin"));

        // add directory to another path
        item = new CommitItem(true, "base1/tags/commit/bin", true);
        _pTree.runItem(item, null);
        assertFalse(_pTree.hasNode("tags/commit/bin"));

        // add directory (no leading slash in path)
        item = new CommitItem(true, "base1/base2/branches/commit/bin", true);
        _pTree.runItem(item, "i2");
        assertEquals(_pTree.getNodeValue("branches/commit/bin"), "i2");

        // delete directory (trailing slash in path)
        item = new CommitItem(true, "base1/base2/trunk/src/");
        item.markAsDelete();
        _pTree.runItem(item, null);
        assertFalse(_pTree.hasNode("trunk/src"));
        assertFalse(_pTree.hasNode("trunk/src/Main.java"));
        assertTrue(_pTree.hasNode("trunk"));

        // delete and readd the root
        item = new CommitItem(true, "/base1/base2", true);
        item.markAsDelete();
        _pTree.runItem(item, null);
        assertTrue(_pTree.hasNode(""));
        
        // delete all tree (both leading and trailing slash in path)
        item = new CommitItem(true, "/base1/");
        item.markAsDelete();
        _pTree.runItem(item, null);
        assertFalse(_pTree.hasNode(""));
        assertFalse(_pTree.hasNode("trunk"));
        assertFalse(_pTree.hasNode("branches"));
    }

    public void testNodeValues() throws Exception
    {
        Map<String, String> list = _pTree.getNodeValues("trunk/src", true, true);
        assertEquals(list.get("trunk/src"), "2");
        assertEquals(list.get("trunk/src/Main.java"), "4");
        assertEquals(list.get("trunk/src/server/Server.java"), "5");
        assertEquals(list.get("trunk/src/server"), "6");
        assertEquals(list.get("trunk/src/config.prop"), "9");

        list = _pTree.getNodeValues("trunk/src", false, true);
        assertNull(list.get("trunk/src"));
        assertEquals(list.get("trunk/src/Main.java"), "4");
        assertEquals(list.get("trunk/src/server/Server.java"), "5");
        assertEquals(list.get("trunk/src/server"), "6");
        assertEquals(list.get("trunk/src/config.prop"), "9");

        list = _pTree.getNodeValues("trunk/src", false, false);
        assertNull(list.get("trunk/src"));
        assertEquals(list.get("trunk/src/Main.java"), "4");
        assertNull(list.get("trunk/src/server/Server.java"));
        assertEquals(list.get("trunk/src/server"), "6");
        assertEquals(list.get("trunk/src/config.prop"), "9");

        list = _pTree.getNodeValues("trunk/srcs", true, true);
        assertEquals(list.size(), 0);

        list = _pTree.getNodeValues("branches/commit/some-dir/some-file", false, false);
        assertEquals(list.size(), 0);

        list = _pTree.getNodeValues("branches/commit/some-dir/some-file", true, true);
        assertEquals(list.get("branches/commit/some-dir/some-file"), "8");
    }
*/

    public static Test suite()
    {
        return new TestSuite(PathTreeTest.class);
    }
}
