package gw.users;


import java.util.*;
import gw.storage.*;


/**
 * The dictionary provides a mapping for user id to all it's aliasses.
 * An alias is a the user itself or a group it belongs to (with
 * transitive closure on group membership).
 */
public interface UserAliasDictionary {
    /**
     * Returns the aliasses of the given user identifier.
     * @param userId The user identifier.
     * @return A list of aliasses (strings).
     */
    public List getAliases(String userId);
    
    
    /**
     * Adds the child alias to the parent alias. This means that the parent
     * becomes an alias of the child. Or in group notion: that the child is
     * a member of the parent.
     * @param parentAlias
     * @param childAlias
     * @throws StorageException
     */
    public void addAliasToAlias(String parentAlias, String childAlias) throws StorageException;
    
    
    /**
     * Removes the child alias from the parent alias. This means that the
     * parent is not an alias of the child anymore. Or in group notion: the
     * child is no longer a member of the parent.
     * @param parentAlias The parent alias.
     * @param childAlias The child alias.
     * @throws StorageException
     */
    public void removeAliasFromAlias(String parentAlias, String childAlias) throws StorageException;
}
