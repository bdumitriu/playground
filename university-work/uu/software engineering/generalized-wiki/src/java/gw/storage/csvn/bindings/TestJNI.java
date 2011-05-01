package gw.storage.csvn.bindings;

import java.util.Iterator;
import gw.storage.*;

/**
 * TestJNI is a test class for testing the SVNJNIAdapter.
 */
public class TestJNI {
    public static void main(String[] args) {
        SVNJNIAdapter s = null;
        try {
            String URI = "https://svn.cs.uu.nl:12443/repos/test-wiki";

            s = new SVNJNIAdapter("gwstorage", "12345", URI);
            // System.out.println(s.fileExists("test", s._sessionPointer));

            // System.out.println(s.isDirectory("ghhg", s._sessionPointer));
            System.out.println(new String(s.getFile("test", SVNJNIAdapter.HEAD_REVISION, s._sessionPointer)));

            Iterator it = s.blame("test", 1, 2, s._sessionPointer).iterator();
            while (it.hasNext()) {
                StorageBlameLine val = (StorageBlameLine) it.next();
                System.out.println("" + val.getRevision() + ", " + val.getAuthor() + ", "
                        + val.getLine());
            }

            /*
             * Iterator it = s.getProperties("COPYING",
             * s.HEAD_REVISION).values().iterator(); while (it.hasNext()) {
             * String val = (String)it.next(); System.out.println(val); }
             */

            /*
             * Iterator it = s.getDirListing("", s.HEAD_REVISION, false,
             * s._sessionPointer).iterator(); while (it.hasNext()) { String val =
             * (String)it.next(); System.out.println(val); }
             */

            // The following code tests property getting/setting:
            /*
             * s.transactionStart("Storage test from JNI code.",
             * s._sessionPointer); System.out.println("Java: entering HenkDir");
             * s.transactionEnterDirectory("HenkDir", s._sessionPointer);
             * System.out.println("Java: modifying dir property 'storage_test'
             * to 'yes'"); s.transactionChangeDirProperty("storage_test", "yes",
             * s._sessionPointer); System.out.println("Java: leaving HenkDir");
             * //s.transactionLeaveDirectory(s._sessionPointer);
             * 
             * System.out.println("Java: committing transaction");
             * s.transactionCommit(s._sessionPointer);
             */

            /*
             * Map m = s.getProperties("", s.HEAD_REVISION, s._sessionPointer);
             * Iterator it = m.entrySet().iterator(); while (it.hasNext()) {
             * Map.Entry entry = (Map.Entry) it.next(); String prop = (String)
             * entry.getKey(); String val = (String) entry.getValue();
             * System.out.println("** Key: " + prop + " ** Value: " + val); }
             */

            /*
             * Tests file copy:
             * 
             * System.out.println("Java: Starting transaction.");
             * s.transactionStart("Storage test from JNI code.",
             * s._sessionPointer); System.out.println("Java: Sending file
             * \"storagetest2\"."); s.transactionCopyFile("storagetest3", URI +
             * "/storagetest2", s._sessionPointer); System.out.println("Java:
             * Committing transaction.");
             * s.transactionCommit(s._sessionPointer);
             * 
             */
        } catch (RAException e) {
            System.out.println(e.toString() + " : " + e.getErrorCode());
        } finally {
            if (s != null) {
                s.closeSession(s._sessionPointer);
            }
        }

        /*
         * Basically, this is how transactions work at the moment:
         * 
         * System.out.println("Java: Starting transaction.");
         * s.transactionStart("Storage test from JNI code.");
         * System.out.println("Java: Sending file \"storagetest2\".");
         * s.transactionSendFile("storagetest2", true);
         * System.out.println("Java: Committing transaction.");
         * s.transactionCommit();
         * 
         * This assumes that there is a file in the local directory called
         * "storagetest2".
         */
    }
}
