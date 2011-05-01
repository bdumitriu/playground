package gw.storage;

//import java.util.Arrays;
//import java.util.Map;

import java.io.File;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import gw.storage.javasvn.PathTree;
import gw.storage.javasvn.SVNRAStorage;
import junit.framework.TestCase;

public class UpdateTest extends TestCase
{
    private SVNRAStorage _storage;
    private SVNRAStorage _otherStorage;

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        _storage = new SVNRAStorage("svn://localhost:10000/branches/update", "gwuser", "gwpassword");
        _storage.setAutoUpdate(false);
        _otherStorage = new SVNRAStorage("svn://localhost:10000/branches/update", "gwuser", "gwpassword");
        
    }

    @Override
    protected void tearDown() throws Exception
    {}

    public void testEmpty() throws Exception
    {}

    public void testUpdate() throws Exception
    {
        PathTree<String> wc = _storage.getWorkingCopy();

        _otherStorage.makeDirectory("games");
        _otherStorage.makeDirectory("games/generator");
        _otherStorage.makeDirectory("games/ifquake");
        _otherStorage.makeDirectory("games/quake3demo");

        _otherStorage.storeFile("games/generator/default.nix").close();
        fillOtherFile(_otherStorage.storeFile("games/generator/soundcard.patch"),
            "games/generator/soundcard.patch");
        fillOtherFile(_otherStorage.storeFile("development/libraries/aterm/aterm-2.3.1.nix"),
            "development/libraries/aterm/aterm-2.3.1.nix");
        fillOtherFile(_otherStorage.storeFile("development/libraries/aterm/default.nix"),
            "development/libraries/aterm/default.nix");

        _otherStorage.commit("");

        long revision = _otherStorage.getLatestRevision();

        fillFile(_storage.storeFile("development/libraries/aterm/default.nix"),
                "development/libraries/aterm/default.nix");

        fillFile(_storage.storeFile("development/libraries/aterm/dummyFile.txt"),
                "development/libraries/aterm/dummyFile.txt");

        File baseFile = wc.getNodeBaseFile("development/libraries/aterm/default.nix");
        assertEquals("Base file before update has wrong size.", 23, baseFile.length());

        wc.registerDeletePath("applications", true);

        assertEquals("Revision number before update is incorrect.", revision, wc.getRevision() + 1);

        _storage.update();

        assertEquals("Revision number after update is incorrect.", revision, wc.getRevision());

        //System.out.println(wc.getNodeChanges("development/libraries/aterm/default.nix").getFile().getAbsolutePath());
        //System.out.println(wc.getNodeBaseFile("development/libraries/aterm/default.nix").getAbsolutePath());

        assertTrue("Locally added file doesn't exist after update.", wc.hasFileNode("development/libraries/aterm/dummyFile.txt"));
        assertTrue("New directory doesn't exist after update.", wc.hasDirNode("games"));
        assertTrue("New directory doesn't exist after update.", wc.hasDirNode("games/generator"));
        assertTrue("New directory doesn't exist after update.", wc.hasDirNode("games/ifquake"));
        assertTrue("New directory doesn't exist after update.", wc.hasDirNode("games/quake3demo"));
        assertTrue("New file doesn't exist after update.", wc.hasFileNode("games/generator/default.nix"));
        assertFalse("Deleted directory reappeared after update.", wc.hasDirNode("applications"));

        // check for file that didn't exist locally 
        assertNull("Non locally modified file was updated and it wasn't supposed to be.",
                wc.getNodeBaseFile("games/generator/soundcard.patch"));

        // check for file that existed locally
        assertNull("Non locally modified file was updated and it wasn't supposed to be.",
                wc.getNodeBaseFile("development/libraries/aterm/aterm-2.3.1.nix"));

        baseFile = wc.getNodeBaseFile("development/libraries/aterm/default.nix");
        assertEquals("Base file after update has wrong size.", baseFile.length(), 55);
        File newFile = wc.getNodeChanges("development/libraries/aterm/default.nix").getFile();
        //FIXME: disabled because the merge has changed to use tags
//        assertEquals("Updated file has wrong size.", newFile.length(), 194);

        assertTrue("File is supposed to be in conflict and it isn't.",
                wc.getNodeConflictState("development/libraries/aterm/default.nix"));

        //printSorted();
    }

    private void fillFile(OutputStream stream, String name) throws Exception
    {
        OutputStreamWriter sw = new OutputStreamWriter(stream);

        if (name.equals("games/generator/soundcard.patch"))
        {
            sw.write("");
        }
        else if (name.equals("development/libraries/aterm/default.nix"))
        {
            sw.write("import and export\n\n" +
                    "import ./aterm-2.4.nix\n" +
                    "some more text...\n");
        }
        else if (name.equals("development/libraries/aterm/dummyFile.txt"))
        {
            sw.write("some text");
        }
        

        sw.close();
    }
    
    private void fillOtherFile(OutputStream stream, String name) throws Exception
    {
        OutputStreamWriter sw = new OutputStreamWriter(stream);
        if (name.equals("games/generator/soundcard.patch"))
        {
            sw.write("diff -rc generator-orig/main/gensoundp-unix.c generator-0.35-cbiere/main/gensoundp-unix.c\n" +
                    "*** generator-orig/main/gensoundp-unix.c    2004-09-26 19:07:44.000000000 +0200\n" +
                    "--- generator-0.35-cbiere/main/gensoundp-unix.c 2004-09-26 19:07:56.000000000 +0200\n" +
                    "***************\n" +
                    "*** 196,202 ****\n" +
                    "  #include <soundcard.h>\n" +
                    "  #else\n" +
                    "\n" +
                    "! #ifdef HAVE_SOUNDCARD_H\n" +
                    "  #include <sys/soundcard.h>\n" +
                    "  #endif\n" +
                    "\n" +
                    "--- 196,202 ----\n" +
                    "  #include <soundcard.h>\n" +
                    "  #else\n" +
                    "\n" + 
                    "! #ifdef HAVE_SYS_SOUNDCARD_H\n" +
                    "  #include <sys/soundcard.h>\n" +
                    "  #endif\n");
        }
        else if (name.equals("development/libraries/aterm/default.nix"))
        {
            sw.write("hello world!\n" +
                    "import ./aterm-2.4.nix\n" +
                    "hello other world!\n");
        }
        else if (name.equals("development/libraries/aterm/aterm-2.3.1.nix"))
        {
            sw.write("some text.");
        }

        sw.close();
    }

//    private void printSorted()
//    {    
//    	    Map<String, String> x = _storage.getWorkingCopy().getNodeValues("", false, true);
//	    String y[] = x.keySet().toArray(new String[0]);
//	    Arrays.sort(y);
//	    for (String z : y)
//	    {
//	        System.out.println(z);
//	    }
//    }
}
