package gw.storage.javahl;

import java.util.*;
import gw.storage.*;
import org.tigris.subversion.javahl.*;


/**
 * A class implementing the Subversion BlameCallback
 */
public class SVNStorageBlameCallback implements BlameCallback
{
    private Collection _blames;


    /**
     * SVNStorageBlameCallback constructor
     *
     */
    public SVNStorageBlameCallback()
    {
        _blames = new ArrayList();
    }

    /**
     * Called for every line in a file
     *
     * @param changed the date of the change
     * @param revision the revision of the change
     * @param author the author responsible for the change
     * @param line the individual line
     */
    public void singleLine(Date changed,
                           long revision,
                           String author,
                           String line)
    {
        StorageBlameLine blameLine = new StorageBlameLine(changed, revision, author, line);
        _blames.add(blameLine);
    }

    /** 
     * Return blame lines
     *
     * @return Collection StorageBlameLine objects
     */
    public Collection getBlameLines()
    {
        return _blames;
    }
}
