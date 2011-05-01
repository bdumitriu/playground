package gw.storage.csvn.bindings;

import java.io.*;
import gw.storage.csvn.*;

public class TestSVNRAStorage {
    public static void main(String[] args) {
        SVNRAStorage s = null;
        try {
            String URI = "file:///svn/repos/gw-test";
            s = new SVNRAStorage(URI, "gwstorage", "12345");

            s.beginTransaction();
            // s.copyFile("nieuw/nogeen/file", "nieuw/file");

            InputStream stream = s.getFile("nieuw/nogeen/file");
            OutputStream out = s.storeFile("testfile");
            while (stream.available() > 0)
                out.write(stream.read());

            out.close();
            stream.close();
            s.endTransaction("");

            stream = s.getFile("testfile");
            while (stream.available() > 0) {
                System.out.print((char) stream.read());
            }

            /*
             * Map m = s.getProperties("nieuw/nogeen/bla"); Iterator it =
             * m.entrySet().iterator(); while (it.hasNext()) { Map.Entry entry =
             * (Map.Entry) it.next(); String prop = (String) entry.getKey();
             * String val = (String) entry.getValue(); System.out.println("**
             * Key: " + prop + " ** Value: " + val); }
             */
            /*
             * Iterator it = s.blame("test", 1, s.HEAD_REVISION); while
             * (it.hasNext()) { StorageBlameLine val =
             * (StorageBlameLine)it.next(); System.out.println("" +
             * val.getRevision()+ ", "+val.getAuthor() + ", " + val.getLine()); }
             */
        } catch (Exception e) {
            System.out.println("Exception: " + e.getMessage());
        }
    }
}
