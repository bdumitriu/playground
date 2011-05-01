package gw.users.acl;

import java.util.List;

/**
 * The resource action represents the action that the user is about to perform
 * on a certain resource.
 * 
 * @author amiddelk
 */
public interface ACLResourceAction {
    /**
     * @return a list of ACLPermission objects representing the required permissions
     *         for this action.
     */
    public List getRequiredPermissions();
}
