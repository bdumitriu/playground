package gw.storage.javasvn;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.tmatesoft.svn.core.ISVNDirEntryHandler;
import org.tmatesoft.svn.core.ISVNLogEntryHandler;
import org.tmatesoft.svn.core.SVNDirEntry;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNLock;
import org.tmatesoft.svn.core.SVNNodeKind;
import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.ISVNFileRevisionHandler;
import org.tmatesoft.svn.core.io.ISVNLocationEntryHandler;
import org.tmatesoft.svn.core.io.ISVNLockHandler;
import org.tmatesoft.svn.core.io.ISVNReporterBaton;
import org.tmatesoft.svn.core.io.ISVNWorkspaceMediator;
import org.tmatesoft.svn.core.io.SVNRepository;

public class RepositoryCache// extends SVNRepository
{	
	private SVNRepository _repository;
	private Map<String, byte[]> _filesCache;
	private Map<String, Map<String, String>> _propertiesCache;
	private Map<String, SVNNodeKind> _checkPathCache;

	private int _filesHits = 0;
	private int _filesMisses = 0;
	private int _propsHits = 0;
	private int _propsMisses = 0;
    private int _checkHits = 0;
    private int _checkMisses = 0;

	public RepositoryCache(SVNRepository repository)
	{
		//super(repository.getLocation(), null);
		_repository = repository;
		_filesCache = new HashMap<String, byte[]>();
		_propertiesCache = new HashMap<String, Map<String, String>>();
		_checkPathCache = new HashMap<String, SVNNodeKind>();
	}

	public int getFilesHits()
	{
		return _filesHits;
	}

	public int getFilesMisses()
	{
		return _filesMisses;
	}

	public int getPropertiesHits()
	{
		return _propsHits;
	}

	public int getPropertiesMisses()
	{
		return _propsMisses;
	}

	public int getCheckHits()
    {
	    return _checkHits;
    }
    
	public int getCheckMisses()
    {
	    return _checkMisses;   
    }

    public void cacheProperties(String path, long revision, Map<String, String> properties)
    {
        String key = path + "@" + revision;
        _propertiesCache.put(key, properties);
    }

    public void cachePath(String path, long revision, SVNNodeKind kind)
    {
        String key = path + "@" + revision;
        _checkPathCache.put(key, kind);
    }
    
	public void checkout(long revision, String target, boolean recursive, ISVNEditor editor) throws SVNException {
		_repository.checkout(revision, target, recursive, editor);
	}

	public SVNNodeKind checkPath(String path, long revision) throws SVNException
	{
	    String key = path + "@" + revision;
	    SVNNodeKind cachedKind = _checkPathCache.get(key);
	    if (cachedKind == null)
	    {
            _checkMisses++;
			cachedKind = _repository.checkPath(path, revision);
			_checkPathCache.put(key, cachedKind);
		}
        else
        {
            _checkHits++;
        }

		return cachedKind;
	}

	public void closeSession() throws SVNException {
		_repository.closeSession();
	}

	@SuppressWarnings("deprecation")
	public void diff(SVNURL url, long targetRevision, long revision, String target, boolean ignoreAncestry, boolean recursive, ISVNReporterBaton reporter, ISVNEditor editor) throws SVNException {
		//_repository.diff(url, revision, target, ignoreAncestry, recursive, reporter, editor);
	}

	@SuppressWarnings("deprecation")
	public void diff(SVNURL url, long revision, String target, boolean ignoreAncestry, boolean recursive, ISVNReporterBaton reporter, ISVNEditor editor) throws SVNException {
		//_repository.diff(url, revision, target, ignoreAncestry, recursive, reporter, editor);
	}

	public ISVNAuthenticationManager getAuthenticationManager() {
		return _repository.getAuthenticationManager();
	}

	public ISVNEditor getCommitEditor(String logMessage, ISVNWorkspaceMediator mediator) throws SVNException {
		return _repository.getCommitEditor(logMessage, mediator);
	}

	public ISVNEditor getCommitEditor(String logMessage, Map locks, boolean keepLocks, ISVNWorkspaceMediator mediator) throws SVNException {
		return _repository.getCommitEditor(logMessage, mediator);
	}

	public long getDatedRevision(Date date) throws SVNException {
		return _repository.getDatedRevision(date);
	}

	public SVNDirEntry getDir(String path, long revision, boolean includeCommitMessages, Collection entries) throws SVNException {
		return _repository.getDir(path, revision, includeCommitMessages, entries);
	}

	public Collection getDir(String path, long revision, Map<String, String> properties, Collection dirEntries) throws SVNException
	{
        //System.out.println("getDir(" + path + ", " + revision + ", " +
        //        properties + ", " + dirEntries + ") called.");
		String key = path + "@" + revision;
		if (properties != null && dirEntries == null)
		{
			Map<String, String> cachedProps = _propertiesCache.get(key);
			if (cachedProps == null)
			{
				_propsMisses++;
				_repository.getDir(path, revision, properties, dirEntries);
				cachedProps = new HashMap<String, String>(properties);
				_propertiesCache.put(key, cachedProps);
			}
			else
			{
				_propsHits++;
				properties.putAll(cachedProps);
			}
		}
		else
		{
			return _repository.getDir(path, revision, properties, dirEntries);
		}

		return new LinkedList();
	}

	public long getDir(String path, long revision, Map properties, ISVNDirEntryHandler handler) throws SVNException {
		return _repository.getDir(path, revision, properties, handler);
	}

	public long getFile(String path, long revision, Map<String, String> properties, OutputStream contents) throws SVNException
	{
        //System.out.println("getFile(" + path + ", " + revision + ", " +
        //        properties + ", " + contents + ") called.");
		String key = path + "@" + revision;
		if (properties == null && contents != null)
		{
			try
			{
				byte[] cachedContents = _filesCache.get(key);
				if (cachedContents == null)
				{
					_filesMisses++;
					ByteArrayOutputStream os = new ByteArrayOutputStream();
					_repository.getFile(path, revision, properties, os);
					os.close();
					cachedContents = os.toByteArray();
					_filesCache.put(key, cachedContents);
				}
				else
				{
					_filesHits++;
				}

				contents.write(cachedContents);
			}
			catch (IOException e)
			{
				throw new SVNException(e.getMessage());
			}
		}
		else if (properties != null && contents == null)
		{
			Map<String, String> cachedProps = _propertiesCache.get(key);
			if (cachedProps == null)
			{
				_propsMisses++;
				_repository.getFile(path, revision, properties, contents);
				cachedProps = new HashMap<String, String>(properties);
				_propertiesCache.put(key, cachedProps);
			}
			else
			{
				_propsHits++;
				properties.putAll(cachedProps);
			}
		}
		else
		{
			return _repository.getFile(path, revision, properties, contents);
		}

		return revision;
	}

	public Collection getFileRevisions(String path, Collection revisions, long sRevision, long eRevision) throws SVNException {
		return _repository.getFileRevisions(path, revisions, sRevision, eRevision);
	}

	public int getFileRevisions(String path, long startRevision, long endRevision, ISVNFileRevisionHandler handler) throws SVNException {
		return _repository.getFileRevisions(path, startRevision, endRevision, handler);
	}

	public String getFullPath(String relativeOrRepositoryPath) {
		return _repository.getFullPath(relativeOrRepositoryPath);
	}

	public long getLatestRevision() throws SVNException {
		return _repository.getLatestRevision();
	}

	public SVNURL getLocation() {
		return _repository.getLocation();
	}

	public Collection getLocations(String path, Collection entries, long pegRevision, long[] revisions) throws SVNException {
		return _repository.getLocations(path, entries, pegRevision, revisions);
	}

	public int getLocations(String path, long pegRevision, long[] revisions, ISVNLocationEntryHandler handler) throws SVNException {
		return _repository.getLocations(path, pegRevision, revisions, handler);
	}

	public Map getLocations(String path, Map entries, long pegRevision, long[] revisions) throws SVNException {
		return _repository.getLocations(path, entries, pegRevision, revisions);
	}

	public SVNLock getLock(String path) throws SVNException {
		return _repository.getLock(path);
	}

	public SVNLock[] getLocks(String path) throws SVNException {
		return _repository.getLocks(path);
	}

	public String getRepositoryPath(String relativePath) {
		return _repository.getRepositoryPath(relativePath);
	}

	public SVNURL getRepositoryRoot() {
		return _repository.getRepositoryRoot();
	}

	public SVNURL getRepositoryRoot(boolean forceConnection) throws SVNException {
		return _repository.getRepositoryRoot(forceConnection);
	}

	public String getRepositoryUUID() {
		return _repository.getRepositoryUUID();
	}

	public Map getRevisionProperties(long revision, Map properties) throws SVNException {
		return _repository.getRevisionProperties(revision, properties);
	}

	public String getRevisionPropertyValue(long revision, String propertyName) throws SVNException {
		return _repository.getRevisionPropertyValue(revision, propertyName);
	}

	public SVNDirEntry info(String path, long revision) throws SVNException {
		return _repository.info(path, revision);
	}

	public void lock(Map pathsToRevisions, String comment, boolean force, ISVNLockHandler handler) throws SVNException {
		_repository.lock(pathsToRevisions, comment, force, handler);
	}

	public Collection log(String[] targetPaths, Collection entries, long startRevision, long endRevision, boolean changedPath, boolean strictNode) throws SVNException {
		return _repository.log(targetPaths, entries, startRevision, endRevision, changedPath, strictNode);
	}

	public long log(String[] targetPaths, long startRevision, long endRevision, boolean changedPath, boolean strictNode, ISVNLogEntryHandler handler) throws SVNException {
		return _repository.log(targetPaths, startRevision, endRevision, changedPath, strictNode, handler);
	}

	public long log(String[] targetPaths, long startRevision, long endRevision, boolean changedPath, boolean strictNode, long limit, ISVNLogEntryHandler handler) throws SVNException {
		return _repository.log(targetPaths, startRevision, endRevision, changedPath, strictNode, limit, handler);
	}

	public void setAuthenticationManager(ISVNAuthenticationManager authManager) {
		_repository.setAuthenticationManager(authManager);
	}

	public void setLocation(SVNURL url, boolean forceReconnect) throws SVNException {
		_repository.setLocation(url, forceReconnect);
	}

	public void setRevisionPropertyValue(long revision, String propertyName, String propertyValue) throws SVNException {
		_repository.setRevisionPropertyValue(revision, propertyName, propertyValue);
	}

	public void status(long revision, String target, boolean recursive, ISVNReporterBaton reporter, ISVNEditor editor) throws SVNException {
		_repository.status(revision, target, recursive, reporter, editor);
	}

	public void testConnection() throws SVNException {
		_repository.testConnection();
	}

	public void unlock(Map pathToTokens, boolean force, ISVNLockHandler handler) throws SVNException {
		_repository.unlock(pathToTokens, force, handler);
	}

	public void update(long revision, String target, boolean recursive, ISVNReporterBaton reporter, ISVNEditor editor) throws SVNException {
		_repository.update(revision, target, recursive, reporter, editor);
	}

	public void update(SVNURL url, long revision, String target, boolean recursive, ISVNReporterBaton reporter, ISVNEditor editor) throws SVNException {
		_repository.update(url, revision, target, recursive, reporter, editor);
	}

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        
        for (Map.Entry<String, Map<String, String>> entry : _propertiesCache.entrySet())
        {
            sb.append("Properties of ");
            sb.append(entry.getKey());
            sb.append("\n");
            for (Map.Entry<String, String> e : entry.getValue().entrySet())
            {
                sb.append(e.getKey());
                sb.append(": ");
                sb.append(e.getValue());
                sb.append("\n");
            }
        }

        return sb.toString();
    }

	
}
