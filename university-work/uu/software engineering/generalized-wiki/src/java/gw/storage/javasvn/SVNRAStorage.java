package gw.storage.javasvn;

import java.io.*;
import java.util.*;

import gw.storage.*;

import org.tmatesoft.svn.core.*;
import org.tmatesoft.svn.core.auth.*;
import org.tmatesoft.svn.core.io.*;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.wc.*;

/**
 * This should eventually become the Storage implementation of the 2005-2006
 * Software Engineering storage team. Of course, when this goal is achieved,
 * this comment should be changed to something more useful.
 *  
 * @author Bogdan Dumitriu
 */
public class SVNRAStorage extends AbstractStorage
{
    /** The URL to the repository */
    private String _repositoryURL;

    /** The user name for connecting to the repository */
    private String _username;

    /** The password for connecting to the repository */
    private String _password;

    /** A JavaSVN link to the repository */
    private RepositoryCache _repository;

    /** The "virtual working copy" */
    private PathTree<String> _workingCopy;
    
    /** temporary file administration */
    private List<File> _tempFiles;

    /** whether or not to do autoupdate */
    private boolean _autoUpdate;

    public SVNRAStorage(String repositoryURL, String username, String password) throws StorageException
    {
        super();

        //System.out.println("repositoryURL: " + repositoryURL);
        //System.out.println("username: " + username);
        //System.out.println("password: " + password);

        _repositoryURL = repositoryURL;
        _username = username;
        _password = password;
        _workingCopy = null;
        _tempFiles = new LinkedList<File>();
        _autoUpdate = true;

        DAVRepositoryFactory.setup();
        SVNRepositoryFactoryImpl.setup();

        initRepository();

        //System.out.println("repositoryURL: " + repositoryURL);
        //System.out.println("username: " + username);
        //System.out.println("password: " + password);
    }

    /**
     * Tries to create the <code>_repository</code>.
     * 
     * @throws StorageException if <code>_repository</code> cannot be successfully created.
     */
    private void initRepository() throws StorageException
    {
        try
        {
            SVNRepository tmpRepo = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(_repositoryURL));
            _repository = new RepositoryCache(tmpRepo);

            ISVNAuthenticationManager authManager =
                SVNWCUtil.createDefaultAuthenticationManager(_username, _password);

            _repository.setAuthenticationManager(authManager);

            if (_workingCopy == null)
            {
                long revision = getLatestRevision();
                SVNRACheckoutEditor editor = new SVNRACheckoutEditor(_repository, revision);
                _repository.checkout(revision, null, true, editor);

//                try
//                {
//                    File f = createTempFile("gw-", "-props");
//                    FileWriter fw = new FileWriter(f);
//                    fw.write(_repository.toString());
//                    fw.close();
//                }
//                catch (Exception e) {}
                
                try
                {
                    _workingCopy = new PathTree<String>(editor.getPaths(), "", revision);
                }
                catch (Exception e)
                {
                    throw new StorageException(e.getMessage());
                }
            }
        }
        catch (SVNException e)
        {
            _repository = null;
            throw new StorageException(e.getMessage());
        }
    }

    /**
     * If:
     * <ul>
     * <li>auto update is set <i>and</i></li>
     * <li>working copy is not fixed <i>and</i></li>
     * <li>working copy's revision is lower than the current head revision</li>
     * </ul>
     * this method calls {@link #update()}.
     */
    private void checkUpdate() throws StorageException
    {
    	    /*
        if (_autoUpdate && !_workingCopy.isFixed())
        {
            long localRevision = _workingCopy.getRevision();
            long headRevision = getLatestRevision();
    
            if (localRevision < headRevision)
            {
                update();
            }
        }
        */
    }

    /**
     * Set to true if you want the working copy to be updated automatically until an
     * edit operation is performed. After an edit operation is performed, even if auto
     * update is set to true, there will be no automatic update.
     */
    public void setAutoUpdate(boolean autoUpdate)
    {
        _autoUpdate = autoUpdate;
    }

    /**
     * Returns the value of auto update. See {@link #setAutoUpdate(boolean)} for details.
     */
    public boolean isAutoUpdate()
    {
        return _autoUpdate;
    }

    public Iterator<StorageBlameLine> blame(String path, long revisionStart,
            long revisionEnd) throws StorageException
    {
        // FIXME these options might be read from a config file somewhere
        // which could possibly give trouble
        ISVNOptions options = null;
        SVNClientManager manager = SVNClientManager.newInstance(options, _username, _password);
        SVNLogClient logClient = manager.getLogClient();
        AnnotateHandler annHandler = new AnnotateHandler();
       
        try
        {
            logClient.doAnnotate(
                    SVNURL.parseURIEncoded(_repositoryURL + path),
                    SVNRevision.create(_workingCopy.getRevision()),
                    SVNRevision.create(revisionStart),
                    SVNRevision.create(revisionEnd),
                    annHandler);
        }
        catch ( SVNException e )
        {
            throw new StorageException(e.getMessage());
        }

        return annHandler.getResultIterator();
    }

    public OutputStream storeFile(String path) throws StorageException
    {
        //System.out.println("storeFile(" + path +") called.");
        checkUpdate();

        path = Path.removeSlashes(path);

        try
        {
            File file = createTempFile("gw-", "");
            long wcRev = _workingCopy.getRevision();
            SVNNodeKind nodeKind = _repository.checkPath(path, wcRev);

            if (nodeKind == SVNNodeKind.FILE)   // modify file
            {
                File baseFile = createTempFile("gw-", "");
                BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(baseFile));
                _repository.getFile(path, wcRev, null, out);
                out.close();

                return new BufferedOutputStream(new SVNRAOutputStream(this, file, path, baseFile));
            }
            else if (nodeKind == SVNNodeKind.NONE)  // add new file
            {
                return new BufferedOutputStream(new SVNRAOutputStream(this, file, path,
                        null));
            }
            else    // nodeKind == SVNNodeKind.DIR
            {
                throw new StorageException("Path '" + path + "' is of type DIR.");
            }
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    /**
     * retrieves a latest revision of a file from the CommitItemContainer or from the Repo
     * 
     * @param path specifies the file path of the file to retrieve
     * 
     * @throws StorageException
     */
    public InputStream getFile(String path) throws StorageException
    {
        //System.out.println("getFile(" + path + ") called.");
        checkUpdate();
        
        String origPath = null;

        CommitItem item;
        try
        {
            item = _workingCopy.getNodeChanges(path);
            origPath = _workingCopy.getRealPath(path);
            origPath = Path.removeSlashes(origPath);
            //System.out.println(origPath);
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }

        if (item != null && item.getFile() != null)
        {
            try
            {
            	return new BufferedInputStream(new FileInputStream(item.getFile()));
            }
            catch (IOException e)
            {
                throw new StorageException(e.getMessage());
            }
        }
        else if (_workingCopy.hasFileNode(path))
        {
            
        	return getFile(origPath, _workingCopy.getRevision());
        }
        else
        {
        	throw new StorageException("There is no entry at '" + path + "'.");
        }
    }

    /**
     * retrieves a specified revision of a file from the Repo
     * 
     * @param path specifies the file path of the file to retrieve
     * @param revision specifies the revision of the file to retrieve
     * 
     * @throws StorageException
     */
    public InputStream getFile(String path, long revision)
            throws StorageException
    {
        //System.out.println("getFile(" + path + ", " + revision + ") called.");
        try
        {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            _repository.getFile(path, revision, null, out);
            out.close();
            
            return new ByteArrayInputStream(out.toByteArray());
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    public Iterator<String> getDirListing(String path) throws StorageException
    {
    	    //System.out.println("getDirListing(" + path + ") called.");
        return getDirListing(path, false);
    }

    public Iterator<String> getDirListing(String path, boolean recurse)
            throws StorageException
    {
    	    //System.out.println("getDirListing(" + path + ", " + recurse + ") called.");
        checkUpdate();
        path = Path.removeSlashes(path);

        if (!(path.equals("") || _workingCopy.hasNode(path)))
        	{
            throw new StorageException("Path '" + path + "' does not exist!");
        	}

        return _workingCopy.getNodeValues(path, false, recurse).keySet().iterator();
    }

    /**
     * This method will return null if the directory indicated by <code>path</code>
     * does not exist. On the other hand, if the directory does exist, but is empty,
     * the method will return an empty Map.
     */
    public Map<String, StorageDirEntry> getDirListing(String path,
            long revision, boolean recurse) throws StorageException
    {
    	    //System.out.println("getDirListing(" + path + ", " + revision + ", " + recurse + ") called.");

    	    path = Path.removeSlashes(path);

        try
        {
            // checks if the paths really is a directory in the specified revision
            SVNNodeKind nodeKind = _repository.checkPath(path, revision);
            if (nodeKind == SVNNodeKind.NONE)
            {
                return null;
            }
            else if (nodeKind == SVNNodeKind.FILE)
            {
                return null;
            }
            else    // nodeKind == SVNNodeKind.DIR
            {
                return getEntries("/" + path, revision, recurse);
            }
        }
        catch (SVNException e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    /**
     * Returns the files and directories under <code>path</code> from the repository.
     * 
     * @param path the root of the directory listing without trailing and leading slashes
     * @param revision the revision to retrieve
     * @param recurse whether to return just the direct children of <code>path</code> or
     *                the entire subtree of <code>path</code>
     * @return a Map from paths to directory entries
     */
    @SuppressWarnings("unchecked")
	private Map<String, StorageDirEntry> getEntries(String path, long revision, boolean recurse)
        throws SVNException
    {
        Map<String, StorageDirEntry> result = new HashMap<String, StorageDirEntry>();

        Collection<SVNDirEntry> entries = _repository.getDir(path, revision, null, (Collection) null);
        
        String basePath = path + "/";
        
        for (SVNDirEntry entry : entries)
        {
            result.put(basePath + entry.getName(), new SVNRADirEntry(basePath, entry));
            
            if (recurse && entry.getKind() == SVNNodeKind.DIR)
            {
                result.putAll(getEntries(basePath + entry.getName(), revision, recurse));
            }
        }

        return result;
    }

    public Map<String, StorageStatusMessage> getStatus(String path)
            throws StorageException
    {
        checkUpdate();

        return getStatus(path, false);
    }

    public Map<String, StorageStatusMessage> getStatus(String path,
            boolean recursive) throws StorageException
    {
        //System.out.println("getStatus(" + path + ", " + recursive + ") called.");
        checkUpdate();
        path = Path.removeSlashes(path);

        HashMap<String, StorageStatusMessage> ssmMap =
            new HashMap<String, StorageStatusMessage>();

        if ( _workingCopy.hasNode(path) )
        {
	        ssmMap.put("/" + path, new SVNRAStorageStatusMessage(
	        		path, _repository, _workingCopy));
	        
            // files are handled seperately because getDirListing gives an empty list
            if ( isDirectory(path) )
            { // directories
                Iterator<String> paths = getDirListing(path, recursive);

                while ( paths.hasNext() )
                {
                    String p = paths.next();

                    ssmMap.put(p, new SVNRAStorageStatusMessage(
                    		p, _repository, _workingCopy));
                }
            }
        }
        else
        {
        	throw new StorageException("Path " + path + " does not exist in the working copy!");
        }

        return ssmMap;
    }

    public boolean fileExists(String path) throws StorageException
    {
        //System.out.println("fileExists(" + path + ") called.");
        checkUpdate();

        return _workingCopy.hasNode(path);
    }

    public boolean isDirectory(String path) throws StorageException
    {
    	    //System.out.println("isDirectory(" + path + ") called.");
        checkUpdate();

        return _workingCopy.hasDirNode(path);
    }

    public void makeDirectory(String pathName) throws StorageException
    {
        //System.out.println("makeDirectory(" + pathName + ") called.");
        checkUpdate();

        try
        {
            _workingCopy.registerMkdir(pathName);
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    public void moveFile(String oldPath, String newPath, boolean force)
            throws StorageException
    {
        checkUpdate();

        try
        {
            _workingCopy.registerMoveFile(oldPath, newPath, force);
        }
        catch (Exception e)
        {
            throw new StorageException(e.toString());
        }
        
    }

    public void copyFile(String originalPath, String copiedPath)
            throws StorageException
    {
        checkUpdate();
        System.out.println("copyFile("+originalPath+","+copiedPath+")");

        try
        {
            _workingCopy.registerCopyFile(originalPath, copiedPath);
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    public void deleteFile(String path, boolean force) throws StorageException
    {
        checkUpdate();

        try
        {
            _workingCopy.registerDeletePath(path, force);
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }
    }

   
    
    public void revertFile(String path) throws StorageException
    {
        try
        {
            _workingCopy.revertFile(path);
        }
        catch(Exception e)
        {
            //System.out.println(e.toString());
            throw new StorageException("Unable to revert " + path);
        }

        // TODO Auto-generated method stub
    }

    public void revertFile(String path, long revision) throws StorageException
    {
        
        try
        {
            SVNNodeKind nodeKind = _repository.checkPath(path, revision);
            
            if(nodeKind == SVNNodeKind.NONE)
            {
                deleteFile(path,true);
            }
            else
            {
                System.out.println("_workingCopy.revertFile("+path+", "+revision+")");
                _workingCopy.revertFile(path, revision);
            }
            
            /*if(nodeKind == SVNNodeKind.FILE)
            {
                
                //should use copy as said in the svn book
                //http://svnbook.red-bean.com/en/1.1/svn-book.html#svn-ch-4-sect-4.3
                       
                try
                {
                    BufferedOutputStream out = new BufferedOutputStream(storeFile(path));
                    BufferedInputStream in = new BufferedInputStream(getFile(path, revision));
                    byte[] buffer = new byte[1024];
                    int length;
                    while ((length = in.read(buffer)) > 0)
                    {
                        out.write(buffer, 0, length);
                    }
                    out.close();
                    in.close();
                }
                catch(IOException e)
                {
                    throw new StorageException("Error while copying streams: " + e.toString());
                }
                catch(StorageException e)
                {
                    throw new StorageException("Trying to revert a file over a directory, please remove directory first and commit");
                }
                
            }
            else if(nodeKind == SVNNodeKind.DIR)
            {
                try
                {
                    _workingCopy.registerMkdir(path);
                }
                catch(NodeExistsException e)
                {
                    //succes
                    //node already exist but that means we can just continue
                    //since that is enough for now (no recursion)
                    //
                    throw new StorageException("NodeExistsException: " + e.toString());
                }
                catch(NodeIsFileException e)
                {
                    throw new StorageException("Trying to revert a directory over a file, please remove the file first and commit");
                }
            }
            else
            {
                deleteFile(path,true);
            }*/
        }
        catch(Exception e)
        {
            throw new StorageException(e.toString());
        }

        // TODO Auto-generated method stub
    }

    public String getFileDiff(String path, long revision1, long revision2)
            throws StorageException
    {
        StringBuilder contents = new StringBuilder();
        
        path = Path.removeSlashes(path);
        String url = Path.removeSlashes(_repositoryURL);

        try
        {
            SVNDiffClient dc;
            dc = new SVNDiffClient(
                SVNWCUtil.createDefaultAuthenticationManager(_username, _password),
                SVNWCUtil.createDefaultOptions(true));
            File f = createTempFile("gw-", "");
            OutputStream os = new BufferedOutputStream(new FileOutputStream( f ));
            dc.doDiff(SVNURL.parseURIEncoded(url + "/" + path), 
                SVNRevision.create(revision1), 
                SVNRevision.create(revision1), 
                SVNRevision.create(revision2), 
                false, 
                false, 
                os);
            
            os.close();
            
            BufferedReader reader = new BufferedReader(new FileReader(f.getAbsolutePath()));

            char[] readBuffer = new char[512];

            int length;
            while ((length = reader.read(readBuffer)) != -1 ) 
            {
                contents.append(readBuffer, 0, length);
            }
        }
        catch (Exception e)
        {
            throw new StorageException("Unable to diff files: " + e.getMessage());
        }

        return contents.toString();
    }

    @SuppressWarnings("unchecked")
	public SortedSet<StorageLogMessage> getLog(String path)
            throws StorageException
    {
    	    //System.out.println("getLog(" + path + ") called.");
        checkUpdate();

        path = Path.removeSlashes(path);

        Collection<SVNLogEntry> entries = new ArrayList<SVNLogEntry>();
        
        try
        {
            entries = _repository.log(new String[] { path }, entries, 1, _workingCopy.getRevision(), true, false);
        }
        catch (SVNException e)
        {
            throw new StorageException(e.getMessage());
        }

        SortedSet<StorageLogMessage> logEntries = new TreeSet<StorageLogMessage>();
        for (SVNLogEntry le : entries)
        {
            logEntries.add(new SVNRALogMessage(le));
        }

        return logEntries;
    }

    public void setProperty(String path, String property, String value,
            boolean recurse) throws StorageException
    {
        //System.out.println("setProperty(" + path + ", " + property + ", " + value + ") called.");
        checkUpdate();
        
        // catch svn properties that we shouldn't allow to be set
        if (  SVNProperty.isEntryProperty(property) || SVNProperty.isSVNProperty(property) )
        	throw new StorageException("Cannot set svn's properties!");

        try
        {
            _workingCopy.registerSetProperty(path, property, value, recurse);
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    public Map<String, String> getProperties(String path)
            throws StorageException
    {
        checkUpdate();

        path = Path.removeSlashes(path);

    	//System.out.println("getProperties(" + path + ") called.");

        Map<String, String> properties = new HashMap<String, String>();

        try
        {
           	long wcRev = _workingCopy.getRevision();
           	SVNNodeKind nodeKind = _repository.checkPath(path, wcRev);
            if (nodeKind == SVNNodeKind.FILE)
            {
                _repository.getFile(path, wcRev, properties, null);
            }
            else if (nodeKind == SVNNodeKind.DIR)
            {
                _repository.getDir(path, wcRev, properties, (Collection) null);
            }
            
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }

        // if it doesn't exist in the repository, then the result depends on
        // whether or not it exists in the container
        if (_workingCopy.hasNode(path))
        {
            try
            {
                if (_workingCopy.getNodeChanges(path) != null
                        && _workingCopy.getNodeChanges(path).isModProperty())
                {
                    Map<String, String> localProps = _workingCopy
                            .getNodeChanges(path).getProperties();
                    properties.putAll(localProps);

                    // NOTE: this is the only way to safely remove items
                    // without invalidating (possibly hidden) iterators
		            Iterator<Map.Entry<String, String>> localPropsIt =
		            	localProps.entrySet().iterator();
		             
		            while ( localPropsIt.hasNext() )
		            {
		            	Map.Entry<String, String> entry = localPropsIt.next();
		            	if ( entry.getValue() == null )
		            	{
		            		localPropsIt.remove();
		            		localProps.remove(entry.getKey());
		            	}
		            }
                }
            }
            catch (Exception e)
            {
                throw new StorageException(e.getMessage());
            }
        }
        else
        {
        	throw new StorageException("Path " + path + " not in repository");
        }
        
        // remove svn properties
        Iterator<String> propIt = properties.keySet().iterator();

         while ( propIt.hasNext() )
         {
        	 String key = propIt.next();
        	 if ( SVNProperty.isEntryProperty(key) || SVNProperty.isSVNProperty(key) )
        	 {
        		propIt.remove();
        	 	properties.remove(key);
        	 }
         }

        //int filesHits = _repository.getFilesHits();
        //int filesMisses = _repository.getFilesMisses();
        //int propsHits = _repository.getPropertiesHits();
        //int propsMisses = _repository.getPropertiesMisses();
        //int checkHits = _repository.getCheckHits();
        //int checkMisses = _repository.getCheckMisses();
        //System.out.println("Files hits: " + filesHits + ", files misses: " + filesMisses + ", props hits: " +
        //        propsHits + ", props misses: " + propsMisses);
        //System.out.println("Check path hits: " + checkHits + ", check path misses: " + checkMisses);

        //System.out.println("Properties of " + path + ":");
        //for (Map.Entry<String, String> entry : properties.entrySet())
        //{
        // 	System.out.println(entry.getKey() + ": " + entry.getValue());
        //}
        
        return properties;
    }

	public boolean commit(String message) throws StorageException
    {
        //System.out.println("commit(" + message + ") called.");
        try
        {
        	//FIXME: this is not a real solution since someone else
        	// might do a commit after this update and before the
        	// actual commit, which will break things.
        	update();
        	
            if (_workingCopy.getConflicts().size() > 0)
            {
            	System.out.println("encountered conflicts, sove those first");
                return false;
            }

            Collection<CommitItem> commitItems = _workingCopy.getCommitItems();
            if (commitItems.size() == 0)
            {
                return true;
            }

            LinkedList<PathTreeNodeData<String>> buildList = new LinkedList<PathTreeNodeData<String>>();
            for (CommitItem ci : commitItems)
            {
                buildList.add(new PathTreeNodeData<String>(ci.getPath(),
                        ci.isFile() ? SVNNodeKind.FILE : SVNNodeKind.DIR));
            }
            
            PathTree<String> commitPT = new PathTree<String>(buildList, "", _workingCopy.getRevision());

            for (CommitItem ci : commitItems)
            {
                commitPT.setNodeChanges(ci.getPath(), ci);
            }

            // initialize the commit editor
            ISVNEditor editor = _repository.getCommitEditor(message,
                    new SVNRAWorkspaceMediator());

            commitPT.commit(editor);

            // open the root directory
            //editor.openRoot(_workingCopy.getRevision());

            // commit all the changes
            //for (CommitItem ci : commitItems)
            //{
                //System.out.println("Committing " + ci.getPath());
                //ci.commit(editor);
            //}

            // close the root directory and the editor itself
            //editor.closeDir();
            //editor.closeEdit();

            // notify the working copy that a successful commit has taken place
            _workingCopy.registerSuccessfulCommit();

            return true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.out.println(e.getMessage());
            throw new StorageException(e.getMessage());
        }
    }

    public void update() throws StorageException
    {
        long headRev = getLatestRevision();
        try
        {
            _repository.update(headRev, null, true,
                    new SVNRAReporterBaton(_workingCopy, _repositoryURL), _workingCopy);
        }
        catch (SVNException e)
        {
            e.printStackTrace();
            System.out.println(e.getMessage());
            throw new StorageException(e.getMessage());
        }
    }

    public Iterator<String> getConflicts() throws StorageException
    {
        checkUpdate();

        return _workingCopy.getConflicts().iterator();
    }

    public void setResolved(String path) throws StorageException
    {
        checkUpdate();

        try
        {
            _workingCopy.setNodeConflictState(path, false);
        }
        catch (Exception e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    public void beginTransaction() throws StorageException
    {}

    public void cancelTransaction() throws StorageException
    {}

    public void endTransaction(String message) throws StorageException
    {}

    public void setUsername(String username) throws StorageException
    {}

    public long getLatestRevision() throws StorageException
    {
        //System.out.println("getLatestRevision() called.");
        try
        {
            return _repository.getLatestRevision();
        }
        catch (SVNException e)
        {
            throw new StorageException(e.getMessage());
        }
    }

    public PathTree<String> getWorkingCopy()
    {
        return _workingCopy;
    }

    private File createTempFile(String prefix, String suffix) throws StorageException
    {
        File tmp;
        try
        {
            tmp = File.createTempFile(prefix, suffix);   
        }
        catch(Exception e)
        {
            throw new StorageException(e.getMessage());
        }
        
        _tempFiles.add(tmp);
        return tmp;
    
    }

    protected void finalize() throws Throwable
    {
        for (File f: _tempFiles)
        {
            f.delete();
        }
    }
}
