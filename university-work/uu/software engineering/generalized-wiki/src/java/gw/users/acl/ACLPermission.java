package gw.users.acl;

/**
 * An action is mapped to a set of required permissions. Such a permission
 * is represented by this interface.
 * A permission accesses a string identity, to be determined by an implementing class.
 */
public interface ACLPermission {
	/**
	 * Gets this permission's identity.
	 * @return A String representing the identity of this Permission.
	 */
    public String getIdentifier();
}
