package gw.storage;

import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.util.Map;

import gw.storage.javasvn.PathTree;
import gw.storage.javasvn.SVNRAStorage;
import junit.framework.*;

/**
 * Test cases for checking the commit method of the gw.storage.javasvn.SVNRAStorage.
 *
 * @author Bogdan Dumitriu
 */
public class CommitTest extends TestCase implements Serializable
{
    /**
	 * Generated version id to get rid of a warning.
	 */
	private static final long serialVersionUID = 1L;

	private transient SVNRAStorage _storage;
    private transient PathTree<String> _wc;

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        _storage = new SVNRAStorage("svn://localhost:10000/", "gwuser", "gwpassword");
        //_storage = new SVNRAStorage("https://svn.cs.uu.nl:12443/repos/test-wiki", "gwstorage", "gwpassword");
        _storage.setAutoUpdate(false);
        _wc = _storage.getWorkingCopy();
    }

    @Override
    protected void tearDown() throws Exception
    {}

    // The tests have to be run in sequence from a main test, so that we can ensure
    // proper serialization
    public void testCommit() throws Exception
    {
        // first add a few new files and directories
        commitTest1();
        //System.out.println("------------------");
        // then change the contents of two files, add a new file and a new directory
        commitTest2();
        //System.out.println("------------------");
        // then change again the contents of a file, and delete a file and a directory
        commitTest3();
        //System.out.println("------------------");
        // now set the properties of a file and of a directory
        commitTest4();
        //System.out.println("------------------");
        // now delete the properties of a file and of a directory
        commitTest5();
        //System.out.println("------------------");
        // now copy a file
        commitTest6();
    }

    private void commitTest1() throws Exception
    {
        assertFalse(_storage.fileExists("branches/commit/src"));
        assertFalse(_storage.fileExists("branches/commit/bin"));
        assertFalse(_storage.fileExists("branches/commit/lib"));
        assertFalse(_storage.fileExists("branches/commit/lib/rt.jar"));
        assertFalse(_storage.fileExists("branches/commit/src/Main.java"));
        
        // new directory
        _storage.makeDirectory("branches/commit/src");

        // new directory
        _storage.makeDirectory("branches/commit/bin");

        // new directory
        _storage.makeDirectory("branches/commit/lib");

        // new empty file
        _storage.storeFile("branches/commit/lib/rt.jar").close();

        // new file with some contents
        OutputStream os = _storage.storeFile("branches/commit/src/Main.java");
        OutputStreamWriter fw = new OutputStreamWriter(os);
        fw.write("public class Main\n{\n\tpublic static void main(String args[])\n\t{\n\t}\n}\n");
        fw.close();

        _storage.commit("Test commit (added some directories and files).");

        assertEquals(_wc.getCommitItems().size(), 0);

        // check the committed files
        assertTrue(_storage.fileExists("branches/commit/src"));
        assertTrue(_storage.fileExists("branches/commit/bin"));
        assertTrue(_storage.fileExists("branches/commit/lib"));
        assertTrue(_storage.fileExists("branches/commit/lib/rt.jar"));
        assertTrue(_storage.fileExists("branches/commit/src/Main.java"));

        byte[] buffer = new byte[1024];
        int length = _storage.getFile("branches/commit/src/Main.java").read(buffer);
        assertEquals("created file did not have expected contents",
                "public class Main\n{\n\tpublic static void main(String args[])\n\t{\n\t}\n}\n",
                new String(buffer, 0, length));
    }

    private void commitTest2() throws Exception
    {
        // modify text file
        OutputStream os = _storage.storeFile("branches/commit/src/Main.java");
        OutputStreamWriter fw = new OutputStreamWriter(os);
        fw.write("public class Main\n{\n\tpublic static void main(String args[])\n\t{\n"
                + "\t\tSystem.out.println(\"Hello world!\");\n\t}\n}\n");
        fw.close();

        // modify binary file
        os = _storage.storeFile("branches/commit/lib/rt.jar");
        ObjectOutputStream oos = new ObjectOutputStream(os);
        oos.writeObject(this);
        oos.close();

        assertFalse(_storage.fileExists("branches/commit/src/server"));
        assertFalse(_storage.isDirectory("branches/commit/src/server"));
        assertFalse(_storage.fileExists("branches/commit/src/server/Server.java"));

        // new directory
        _storage.makeDirectory("branches/commit/src/server");

        // new empty file
        _storage.storeFile("branches/commit/src/server/Server.java").close();

        _storage.commit("Test commit (modified some files and added a new directory and a new file).");

        assertEquals(_wc.getCommitItems().size(), 0);

        // check the committed files
        assertTrue(_storage.fileExists("branches/commit/lib/rt.jar"));
        assertTrue(_storage.fileExists("branches/commit/src/Main.java"));
        assertTrue(_storage.fileExists("branches/commit/src/server"));
        assertTrue(_storage.fileExists("branches/commit/src/server/Server.java"));

        byte[] buffer = new byte[1024];
        int length = _storage.getFile("branches/commit/src/Main.java").read(buffer);
        assertEquals("modified file did not have expected contents",
                "public class Main\n{\n\tpublic static void main(String args[])\n\t{\n"
                + "\t\tSystem.out.println(\"Hello world!\");\n\t}\n}\n",
                new String(buffer, 0, length));

        length = _storage.getFile("branches/commit/src/server/Server.java").read(buffer);
        assertEquals("expectedly empty file had some contents", length, -1);
    }

    private void commitTest3() throws Exception
    {
        // modify text file
        OutputStream os = _storage.storeFile("branches/commit/src/Main.java");
        OutputStreamWriter fw = new OutputStreamWriter(os);
        fw.write("public class Main\n{\n\tpublic static void main(String arguments[])\n\t{\n"
                + "\t\tSystem.out.println(arguments[0]);\n\t}\n}\n");
        fw.close();

        // delete file
        _storage.deleteFile("branches/commit/src/server/Server.java", true);

        // delete directory
        _storage.deleteFile("branches/commit/src/server", true);

        _storage.commit("Test commit (modified a file and deleted a directory).");

        assertEquals(_wc.getCommitItems().size(), 0);

        // check committed files
        assertTrue(_storage.fileExists("branches/commit/src/Main.java"));
        assertFalse(_storage.fileExists("branches/commit/src/server"));
        assertFalse(_storage.fileExists("branches/commit/src/server/Server.java"));

        byte[] buffer = new byte[1024];
        int length = _storage.getFile("branches/commit/src/Main.java").read(buffer);
        assertEquals("modified file did not have expected contents",
                "public class Main\n{\n\tpublic static void main(String arguments[])\n\t{\n"
                + "\t\tSystem.out.println(arguments[0]);\n\t}\n}\n",
                new String(buffer, 0, length));
    }

    private void commitTest4() throws Exception
    {
        // modify file properties
        _storage.setProperty("branches/commit/src/Main.java", "author",
                "Bogdan Dumitriu", false);
        _storage.setProperty("branches/commit/src/Main.java", "since",
                "version 0.1", false);

        // modify directory properties
        _storage.setProperty("branches/commit/lib", "since", "version 0.1", false);

        // change root's properties
        _storage.setProperty("", "since", "version 0.1", false);

        _storage.commit("Test commit (changed the properties of a file and of a directory).");

        assertEquals(_wc.getCommitItems().size(), 0);

        // check committed files
        Map<String, String> props = _storage.getProperties("branches/commit/src/Main.java");

        assertEquals(props.get("author"), "Bogdan Dumitriu");
        assertEquals(props.get("since"), "version 0.1");

        props = _storage.getProperties("branches/commit/lib");
        assertEquals(props.get("since"), "version 0.1");

        props = _storage.getProperties("");
        assertEquals(props.get("since"), "version 0.1");
    }

    private void commitTest5() throws Exception
    {
        // modify file properties
        _storage.setProperty("branches/commit/src/Main.java", "author", null, false);
        _storage.setProperty("branches/commit/src/Main.java", "since", null, false);
        _storage.setProperty("branches/commit/src/Main.java", "priority", "1", false);

        // modify directory properties
        _storage.setProperty("branches/commit/lib", "since", null, false);
        _storage.setProperty("branches/commit/lib", "priority", "12", false);

        _storage.commit("Test commit (deleted the properties of a file and of a directory).");

        assertEquals(_wc.getCommitItems().size(), 0);

        // check committed files
        Map<String, String> props = _storage.getProperties("branches/commit/src/Main.java");

        assertFalse(props.containsKey("author"));
        assertFalse(props.containsKey("since"));
        assertEquals(props.get("priority"), "1");

        props = _storage.getProperties("branches/commit/lib");

        assertFalse(props.containsKey("since"));
        assertEquals(props.get("priority"), "12");
    }

    private void commitTest6() throws Exception
    {
        _storage.copyFile("branches/commit/src/Main.java", "branches/commit/Main.java");
        _storage.copyFile("branches/commit/src/Main.java", "branches/commit/lib/Main.java");
        
        OutputStream os = _storage.storeFile("branches/commit/lib/Main.java");
        OutputStreamWriter fw = new OutputStreamWriter(os);
        fw.write("some text");
        fw.close();

        _storage.commit("Test commit (copy file)");
        
        assertEquals(_wc.getCommitItems().size(), 0);

        assertTrue(_storage.fileExists("branches/commit/Main.java"));
        assertTrue(_storage.fileExists("branches/commit/lib/Main.java"));

        byte[] buffer = new byte[1024];
        int length = _storage.getFile("branches/commit/Main.java").read(buffer);
        assertNotSame("copied file was empty", -1, length);
        assertEquals("copied file did not have expected contents",
                "public class Main\n{\n\tpublic static void main(String arguments[])\n\t{\n"
                + "\t\tSystem.out.println(arguments[0]);\n\t}\n}\n",
                new String(buffer, 0, length));

        length = _storage.getFile("branches/commit/lib/Main.java").read(buffer);
        assertNotSame("copied and changed file was empty", -1, length);
        assertEquals("copied and changed file did not have expected contents",
                "some text",
                new String(buffer, 0, length));
    }

    public static Test suite()
    {
        return new TestSuite(CommitTest.class);
    }
}
