package gw.storage.javasvn;

import java.util.HashMap;
import java.util.Collection;
import java.util.Map;
//import java.util.Set;

/**
 * A container for managing commit items. This class provides a lot of functionality
 * related to what you would expect of a working copy. It's a sort of working copy simulation
 * which doesn't really check out all the files at once, but rather on a per-request basis. 
 *
 * @author Bogdan Dumitriu
 */
public class CommitItemContainer
{
    private HashMap<String, CommitItem> _items;
    
    /** A JavaSVN link to the repository */
    //private SVNRepository _repository;

    /**
     * Creates a new container for commit items.
     */
    public CommitItemContainer()
    {
        _items = new HashMap<String, CommitItem>();
        //_repository = repository;
    }

    /**
     * Adds a commit item to the container.
     */
    public void add(CommitItem item)
    {
        _items.put(item.getPath(), item);
    }

    /**
     * Removes a commit item from the container. 
     */
    public void remove(String path)
    {
        _items.remove(path);
    }

    /**
     * Retrieves a commit item from the container.
     */
    public CommitItem getCommitItem(String path)
    {
        CommitItem item = (CommitItem)_items.get(path);
        return item;
    }

    /**
     * Returns a Map with all the CommitItems that refer to a path which is
     * either <code>path</code> itself, or a subdirectory thereof. Note that
     * the returned commit items could also indicate deletion of certain paths,
     * so looking only at the entries' keys is probably not enough for most
     * purposes.
     */
    public Map<String, CommitItem> getCommitItems(String path)
    {
        HashMap<String, CommitItem> result = new HashMap<String, CommitItem>();
        for (Map.Entry<String, CommitItem> entry : _items.entrySet())
        {
            if (entry.getKey().startsWith(path))
            {
                result.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    /**
     * Checks if a commit item for this path exists in the container.
     */
    public boolean exists(String path)
    {
        return _items.containsKey(path);
    }

    /**
     * Checks if the container contains any commit items.
     */
    public boolean isEmpty()
    {
        return (_items.isEmpty());
    }
    
    /**
     * Removes all items from the container.
     */
    public void clear()
    {
        _items.clear();
    }

    /**
     * Returns all the commit items in the container.
     */
    public Collection<CommitItem> values()
    {
        return _items.values();
    }
}
