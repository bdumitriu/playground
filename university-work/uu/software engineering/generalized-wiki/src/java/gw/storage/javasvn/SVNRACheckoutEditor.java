package gw.storage.javasvn;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;

import org.tmatesoft.svn.core.SVNCommitInfo;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNNodeKind;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.diff.SVNDiffWindow;

/**
 * A custom implementation of ISVNEditor.
 *
 * @author Gideon Smeding
 * @author Bogdan Dumitriu
 */
public class SVNRACheckoutEditor implements ISVNEditor
{
    private ArrayList<PathTreeNodeData<String>> _paths = new ArrayList<PathTreeNodeData<String>>();
    private RepositoryCache _cache;
    private long _revision;
    private LinkedList<HashMap<String, String>> _runningProperties;
    private LinkedList<String> _runningPath;

    public SVNRACheckoutEditor(RepositoryCache cache, long revision)
    {
        _cache = cache;
        _revision = revision;
    }

    public void targetRevision(long revision) throws SVNException
    {}

    public void openRoot(long revision) throws SVNException
    {
        _runningPath = new LinkedList<String>();
        _runningProperties = new LinkedList<HashMap<String, String>>();

        _runningPath.add("");
        _runningProperties.add(new HashMap<String, String>());

        _cache.cachePath("", _revision, SVNNodeKind.DIR);
    }

    public void deleteEntry(String path, long revision) throws SVNException
    {}

    public void absentDir(String path) throws SVNException
    {}

    public void absentFile(String path) throws SVNException
    {}

    public void addDir(String path, String copyFromPath, long copyFromRevision)
            throws SVNException
    {
        _runningPath.add(path);
        _runningProperties.add(new HashMap<String, String>());
        _paths.add(new PathTreeNodeData<String>(path, SVNNodeKind.DIR));
        _cache.cachePath(path, _revision, SVNNodeKind.DIR);
    }

    public void openDir(String path, long revision) throws SVNException
    {
        _runningPath.add(path);
        _runningProperties.add(new HashMap<String, String>());
        _cache.cachePath(path, _revision, SVNNodeKind.DIR);
    }

    public void changeDirProperty(String name, String value)
            throws SVNException
    {
        _runningProperties.getLast().put(name, value);
    }

    public void closeDir() throws SVNException
    {
        _cache.cacheProperties(_runningPath.removeLast(), _revision,
                _runningProperties.removeLast());
    }

    public void addFile(String path, String copyFromPath, long copyFromRevision)
            throws SVNException
    {
        _runningPath.add(path);
        _runningProperties.add(new HashMap<String, String>());
        _paths.add(new PathTreeNodeData<String>(path, SVNNodeKind.FILE));
        _cache.cachePath(path, _revision, SVNNodeKind.FILE);
    }

    public void openFile(String path, long revision) throws SVNException
    {
        _runningPath.add(path);
        _runningProperties.add(new HashMap<String, String>());
        _cache.cachePath(path, _revision, SVNNodeKind.FILE);
    }

    public void applyTextDelta(String path, String baseChecksum)
            throws SVNException
    {}

    public OutputStream textDeltaChunk(String path, SVNDiffWindow diffWindow)
            throws SVNException
    {
        return null;
    }

    public void textDeltaEnd(String path) throws SVNException
    {}

    public void changeFileProperty(String path, String name, String value)
            throws SVNException
    {
        _runningProperties.getLast().put(name, value);
    }

    public void closeFile(String path, String textChecksum) throws SVNException
    {
        _cache.cacheProperties(_runningPath.removeLast(), _revision,
                _runningProperties.removeLast());
    }

    public SVNCommitInfo closeEdit() throws SVNException
    {
        return null;
    }

    public void abortEdit() throws SVNException
    {}

    public ArrayList<PathTreeNodeData<String>> getPaths()
    {
        return _paths;
    }
}
