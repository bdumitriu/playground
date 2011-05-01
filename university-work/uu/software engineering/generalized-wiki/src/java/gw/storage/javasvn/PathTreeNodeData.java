package gw.storage.javasvn;

import org.tmatesoft.svn.core.SVNNodeKind;

/**
 * This class encapsulates data which is associated with a node in PathTree.
 * 
 * @author Bogdan Dumitriu
 */
public class PathTreeNodeData<V> implements Comparable
{
    private String _path;
    private SVNNodeKind _type;
    private V _value;

    /**
     * Equivalent to PathTreeNodeData(path, type, null).
     *
     * @param path the path of this node
     * @param type the type of this node (SVNNodeKind.File or SVNNodeKind.Dir)
     */
    public PathTreeNodeData(String path, SVNNodeKind type)
    {
        this(path, type, null);
    }

    /**
     * Builds a new PathTreeNodeData with the specified values.
     *
     * @param path the path of this node
     * @param type the type of this node (SVNNodeKind.File or SVNNodeKind.Dir)
     * @param value the value associated with this node
     */
    public PathTreeNodeData(String path, SVNNodeKind type, V value)
    {
        _path = path;
        _type = type;
        _value = value;
    }

    public String getPath()
    {
        return _path;
    }

    public void setPath(String path)
    {
        _path = path;
    }

    public V getValue()
    {
        return _value;
    }

    public void setValue(V value)
    {
        _value = value;
    }

    public SVNNodeKind getType()
    {
        return _type;
    }

    public void setType(SVNNodeKind type)
    {
        _type = type;
    }

    public int compareTo(Object o)
    {
        if (o == null || !(o instanceof PathTreeNodeData))
        {
            return -1;
        }

        PathTreeNodeData obj = (PathTreeNodeData) o;
        if (_type != obj._type)
        {
            return _type.compareTo(obj._type);
        }
        else if (_path == null || obj._path == null)
        {
            return _path == obj._path ? 0 : (_path == null ? -1 : 1); 
        }
        else
        {
            return _path.compareTo(obj._path);
        }
    }
}
