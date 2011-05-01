package gw.users.acl;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Provides a resource action.
 */
class NamedACLResourceAction implements ACLResourceAction {
    private ACLPermission[] _permissions;
    
    /**
	 * @param permissions permission for the action.
	 */
    public NamedACLResourceAction(final ACLPermission[] permissions) {
        if (permissions == null)
            throw new IllegalArgumentException("ACLPermissions is null.");
        if (permissions.length <= 0)
            throw new IllegalArgumentException("# ACLPermissions <= 0");
        
        _permissions = permissions;
    }
    
    /**
	 * @return The name of the action
	 */
    public List getRequiredPermissions() {
        return Collections.unmodifiableList(Arrays.asList(_permissions));
    }
}