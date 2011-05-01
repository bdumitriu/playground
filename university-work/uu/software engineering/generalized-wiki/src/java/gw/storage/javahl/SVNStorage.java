package gw.storage.javahl;

import java.io.*;
import java.util.*;
import gw.storage.*;
import org.tigris.subversion.javahl.*;

/**
 * A class implementing the Storage interface with the Subversion javahl
 * bindings. Each SVNStorage has a temporary working copy on which it works. For
 * a less expensive implementation use the RA implementation
 * 
 * 
 */
public class SVNStorage extends AbstractStorage {
    /* subversion apr error, from C code */
    private static final int MERGE_CONFLICT = 160024;
    private String _workingCopy;
    private String _repository;

    /**
     * The Subversion client object
     */
    protected final SVNClientInterface _client = new SVNClientSynchronized();

    /**
     * Constructor
     * 
     * @param repository
     *            Complete URL to the repository containing the GWiki
     * @param username
     *            The username for whom this SVNStorage is constructed, also
     *            used for auth. purposes
     * @param password
     *            Corresponding password for username
     */
    public SVNStorage(String repository, String username, String password) throws StorageException {
        _repository = repository;
        _workingCopy = createWorkingCopy();

        setUsername(username);
        getSVNClient().password(password);
        checkout();
    }

    /**
     * @see Storage#blame
     */
    public Iterator blame(String path, long revisionStart, long revisionEnd)
            throws StorageException {
        try {
            SVNStorageBlameCallback blameCallback = new SVNStorageBlameCallback();
            getSVNClient().blame(getWorkingCopy() + path, Revision.getInstance(revisionStart),
                    Revision.getInstance(revisionEnd), blameCallback);
            return blameCallback.getBlameLines().iterator();
        } catch (ClientException ex) {
            throw new StorageException("Unable to get blame: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#storeFile
     */
    public OutputStream storeFile(String path) throws StorageException {
        SVNOutputStream stream;

        try {
            File file = new File(getWorkingCopy(), path);
            stream = new SVNOutputStream(this, file);
        } catch (IOException ex) {
            throw new SVNStorageException("Unable to store file: " + ex.getMessage());
        }

        return stream;
    }

    /**
     * @see Storage#getFile(String)
     */
    public InputStream getFile(String path) throws StorageException {
        InputStream stream = null;

        try {
            File file = new File(getWorkingCopy(), path);
            stream = new FileInputStream(file);
        } catch (IOException ex) {
            throw new StorageException("Unable to get file: " + ex.getMessage());
        }

        return stream;
    }

    /**
     * @see Storage#getFile(String, long)
     */
    public InputStream getFile(String path, long revision) throws StorageException {
        ByteArrayInputStream stream = null;

        try {
            byte[] data = getSVNClient().fileContent(getWorkingCopy() + path,
                    Revision.getInstance(revision));
            stream = new ByteArrayInputStream(data);
        } catch (ClientException ex) {
            throw new SVNStorageException("Unable to get file: " + ex.getMessage());
        }

        return stream;
    }

    /**
     * @see Storage#getDirListing(String)
     */
    public Iterator getDirListing(String path) throws StorageException {
        return getDirListing(path, false);
    }

    /**
     * @see Storage#getDirListing(String, boolean)
     */
    public Iterator getDirListing(String path, boolean recurse) throws StorageException {
        Status[] listing = null;
        List dirListing = new ArrayList();

        try {
            listing = getSVNClient().status(getWorkingCopy() + path, recurse, false, true);
        } catch (ClientException ce) {
            throw new SVNStorageException("Unable to get dir listing: " + ce.getMessage());
        }

        for (int i = 1; i < listing.length; i++) {
            // index 0 is skipped

            Status st = listing[i];

            if (!st.isDeleted() && st.isManaged()) {
                dirListing.add(st.getPath().replaceFirst(getWorkingCopy(), ""));
            }

        }

        return dirListing.iterator();
    }

    /**
     * @see Storage#getDirListing(String, long, boolean)
     */
    public Map getDirListing(String path, long revision, boolean recurse) throws StorageException {
        DirEntry[] entries = null;
        Map dirListing = new HashMap();

        try {
            entries = getSVNClient().list(getRepository() + path, Revision.getInstance(revision),
                    recurse);
        } catch (ClientException ce) {
            throw new SVNStorageException("Unable to get dir. listing for path: " + path
                    + " with revision: " + revision + "\nSVN: " + ce.getMessage());
        }

        for (int i = 0; i < entries.length; i++) {
            dirListing.put(entries[i].getPath().replaceFirst(getWorkingCopy(), ""),
                    new SVNStorageDirEntry(entries[i]));
        }

        return dirListing;
    }

    /**
     * @see Storage#getStatus(String, boolean)
     */
    public Map getStatus(String path, boolean recursive) throws SVNStorageException {
        Status[] stats = null;
        Map<String, StorageStatusMessage> fileToStatus = new TreeMap<String, StorageStatusMessage>();

        try {
            stats = getSVNClient().status(getWorkingCopy() + path, recursive, false, true);
        } catch (ClientException ce) {
            throw new SVNStorageException("Unable to get status for path: " + path + "\nSVN:"
                    + ce.getMessage());
        }

        for (int i = 0; i < stats.length; i++) {

            Status st = stats[i];

            if (st.isManaged()) {
                fileToStatus.put(st.getPath().replaceFirst(getWorkingCopy(), ""),
                        new SVNStorageStatusMessage(st));
            }

        }

        return fileToStatus;
    }

    /**
     * @see Storage#getStatus(String)
     */
    public Map<String, StorageStatusMessage> getStatus(String path) throws SVNStorageException {
        return getStatus(path, false);
    }

    /**
     * @see Storage#fileExists
     */
    public boolean fileExists(String path) throws StorageException {
        File file = new File(getWorkingCopy(), path);
        return file.exists();
    }

    /**
     * @see Storage#isDirectory
     */
    public boolean isDirectory(String path) throws StorageException {
        /*
         *  // NOTE!! IMPORTANT!! // this part is commented out for a reason: //
         * status() is broken in JavaHL subversion bindings. It reports
         * directories // to be non-directories, when there is a conflict in the
         * workingcopy. // A workaround using java.io.File is implemented for
         * now. This might not be // fixed...
         * 
         * Status[] status; try { status =
         * getSVNClient().status(getWorkingCopy() + path, false, false, true);
         * if (status.length < 1) { System.out.println("Can't get status");
         * return false; } } catch (ClientException ex) { throw new
         * SVNStorageException(ex.toString()); }
         * 
         * if (status[0].getNodeKind() == NodeKind.dir && status[0].isManaged()) {
         * return true; }
         * 
         * return false;
         */

        File f = new File(getWorkingCopy() + path);
        return f.isDirectory();

    }

    /**
     * @see Storage#makeDirectory
     */
    public void makeDirectory(String pathName) throws StorageException {
        try {
            String[] path = new String[1];
            path[0] = getWorkingCopy() + pathName;
            getSVNClient().mkdir(path, "");
        } catch (ClientException ex) {
            throw new SVNStorageException("Unable to add directory: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#moveFile
     */
    public void moveFile(String oldPath, String newPath, boolean force) throws StorageException {
        try {
            getSVNClient().move(getWorkingCopy() + oldPath, getWorkingCopy() + newPath, null, null,
                    force);

            fireFileMoved(oldPath, newPath);
        } catch (ClientException ex) {
            throw new SVNStorageException("Unable to move file: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#copyFile
     */
    public void copyFile(String originalPath, String copiedPath) throws StorageException {
        try {
            getSVNClient().copy(getWorkingCopy() + originalPath, getWorkingCopy() + copiedPath,
                    null, null);

            fireFileAdded(copiedPath);
        } catch (ClientException ex) {
            throw new SVNStorageException("Unable to move file: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#deleteFile
     */
    public void deleteFile(String path, boolean force) throws StorageException {
        String[] singlePath = new String[1];
        singlePath[0] = getWorkingCopy() + path;

        try {
            getSVNClient().remove(singlePath, "", force);

            fireFileDeleted(path);
        } catch (ClientException ce) {
            throw new SVNStorageException("Unable to delete file: " + path + " SVN: "
                    + ce.getMessage());
        }
    }

    /**
     * @see Storage#revertFile(String)
     */
    public void revertFile(String path) throws StorageException {
        try {
            getSVNClient().revert(getWorkingCopy() + path, false);
        } catch (ClientException ex) {
            throw new SVNStorageException("Unable to revert file: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#revertFile(String, long)
     */
    public void revertFile(String path, long revision) throws StorageException {
        try {
            byte[] data = getSVNClient().fileContent(getWorkingCopy() + path,
                    Revision.getInstance(revision));
            OutputStream stream = storeFile(path);
            stream.write(data);
            stream.close();
        } catch (Exception ex) {
            throw new SVNStorageException("Unable to revert file: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#getFileDiff
     */
    public String getFileDiff(String path, long revision1, long revision2) throws StorageException {
        File target = new File(getWorkingCopy(), path);

        Revision rev1 = Revision.getInstance(revision1);
        Revision rev2 = Revision.getInstance(revision2);

        try {
            File tempfile = File.createTempFile("gw-diff", null);

            getSVNClient().diff(target.getAbsolutePath(), rev1, target.getAbsolutePath(), rev2,
                    tempfile.getAbsolutePath(), false);

            String contents = new String();

            BufferedReader reader = new BufferedReader(new FileReader(tempfile.getAbsolutePath()));

            while (reader.ready()) {
                String line = reader.readLine();

                contents = contents.concat(line + "\n");
            }

            return contents;
        } catch (Exception ex) {
            throw new SVNStorageException("Unable to diff files: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#getLog
     */
    public SortedSet getLog(String path) throws StorageException {
        LogMessage messages[];

        try {
            messages = getSVNClient().logMessages(getWorkingCopy() + path, Revision.START,
                    Revision.HEAD, false, true);
        } catch (ClientException ce) {
            throw new SVNStorageException("Unable to get log messages: " + ce.getMessage());
        }

        TreeSet logs = new TreeSet();

        for (int i = 0; i < messages.length; ++i) {
            logs.add(new SVNStorageLogMessage(messages[i]));
        }

        return logs;
    }

    /**
     * @see Storage#setProperty
     */
    public void setProperty(String path, String property, String value, boolean recurse)
            throws StorageException {
        try {
            getSVNClient().propertySet(getWorkingCopy() + path, property, value, recurse);

            // don't do this: only fire events of things that are already
            // commited.
            // fireFileModified(path);
        } catch (ClientException ce) {
            throw new StorageException("unable to set property: " + property + " with value: "
                    + value);
        }

    }

    /**
     * @see Storage#getProperties
     */
    public Map getProperties(String path) throws StorageException {

        Map properties = new HashMap();

        try {

            PropertyData[] pd = getSVNClient().properties(getWorkingCopy() + path);

            for (int i = 0; i < pd.length; i++) {
                properties.put(pd[i].getName(), pd[i].getValue());
            }

        } catch (ClientException ce) {
            throw new SVNStorageException("Unable to get properties for: " + path + " SVN: "
                    + ce.getMessage());
        } catch (NullPointerException npe) {
        }

        return properties;

    }

    /**
     * @see Storage#commit
     */
    public boolean commit(String message) throws StorageException {
        try {
            if (getWorkingCopy() == null) {
                return false; // No working copy? No commit.
            }

            String[] pathname = new String[1];
            pathname[0] = getWorkingCopy();

            long revision = getSVNClient().commit(pathname, message, true);
            
            if (revision == -1) {
                throw new SVNStorageException("Commit failed.");
            }

            // Request the status of all changed files, to fire appropriate
            // events
            Status[] status = getSVNClient().status(pathname[0], true, false, false);
            for (int i = 0; i < status.length; ++i) {
                if (!status[i].isManaged()) {
                    continue;
                }

                if (status[i].isAdded()) {
                    fireFileAdded(status[i].getPath().replaceFirst(getWorkingCopy(), ""));
                } else if (status[i].isModified()) {
                    fireFileModified(status[i].getPath().replaceFirst(getWorkingCopy(), ""));
                } else if (status[i].isDeleted()) {
                    fireFileDeleted(status[i].getPath().replaceFirst(getWorkingCopy(), ""));
                }
            }
        } catch (ClientException ex) {

            if (ex.getAprError() == MERGE_CONFLICT) // MERGE_CONFLICT contains
                                                    // the APR error #
                                                    // associated with merge
                                                    // conflicts
            {
                return false;
            }
            throw new SVNStorageException("Unable to commit: " + ex.getMessage());
        }

        return true;
    }

    /**
     * @see Storage#update
     */
    public void update() throws StorageException {
        try {
            getSVNClient().update(getWorkingCopy(), Revision.HEAD, true);
        } catch (ClientException ex) {
            throw new SVNStorageException("Unable to update: " + ex.getMessage());
        }
    }

    /**
     * @see Storage#getConflicts
     */
    public Iterator getConflicts() throws StorageException {
        Collection list = new ArrayList();

        try {
            this.update();
            Status[] stats = getSVNClient().status(getWorkingCopy(), true, false, false);

            // add conflicting files to collection
            for (int i = 0; i < stats.length; i++) {
                if (stats[i].getConflictNew() != null) {
                    String filename = stats[i].getPath() + "/";

                    list.add(filename.replaceFirst(getWorkingCopy(), ""));
                }
            }
        } catch (ClientException ex) {
            throw new SVNStorageException("Unable to get conflicting filenames:" + ex.getMessage());
        }

        return list.iterator();
    }

    /**
     * @see Storage#setResolved
     */
    public void setResolved(String path) throws StorageException {
        try {
            getSVNClient().resolved(getWorkingCopy() + path, true);
        } catch (ClientException ex) {
            throw new StorageException("Unable to resolve file: " + ex.getMessage());
        }

    }

    /**
     * Checkout to _workingCopy using _repository
     */
    private void checkout() throws StorageException {
        try {
            getSVNClient().checkout(getRepository(), getWorkingCopy(), Revision.HEAD, true);
        } catch (ClientException ce) {
            throw new SVNStorageException("Unable to checkout repository: " + ce.getMessage());
        }
    }

    /**
     * Creates a temporary directory for the _workingcopy to reside in.
     * 
     * @return the path to the temporary directory
     */
    private String createWorkingCopy() throws StorageException {
        File result = null;
        try {
            int tries = 256;

            while (result == null && --tries > 0) {
                result = File.createTempFile("gw-working-copy", null, null);
                if (!result.delete() || !result.mkdirs())
                    result = null;
            }

            if (result == null)
                throw new StorageException("Unable to create working copy");
        } catch (IOException ex) {
            throw new StorageException("Unable to create working copy:" + ex.getMessage());
        }

        return result.getAbsolutePath();
    }

    /**
     * @see Storage#cancelTransaction
     */
    public void cancelTransaction() throws StorageException {
        try {
            getSVNClient().revert(getWorkingCopy() + "/", true);
        } catch (ClientException ce) {
            throw new StorageException("Unable to cancel transaction: " + ce.getMessage());
        }
    }

    /**
     * @see Storage#beginTransaction
     */
    public void beginTransaction() throws StorageException {
    }

    /**
     * @see Storage#endTransaction
     */
    public void endTransaction(String message) throws StorageException {
        commit(message);
    }

    /**
     * Returns the SVN Client interface.
     */
    protected SVNClientInterface getSVNClient() {
        return _client;
    }

    /**
     * Returns the path to the working copy
     */
    protected String getWorkingCopy() {
        return _workingCopy;
    }

    /**
     * Returns the path/URL to the repository
     */
    protected String getRepository() {
        return _repository;
    }

    /**
     * Sets the SVN username.
     * 
     * @see Storage#setUsername(String)
     */
    public void setUsername(String username) throws StorageException {
        getSVNClient().username(username);
    }
}
