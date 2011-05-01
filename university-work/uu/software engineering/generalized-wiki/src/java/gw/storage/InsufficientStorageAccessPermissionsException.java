package gw.storage;


/**
 * If this exception is thrown, you do not have sufficient rights to perform a certain action.
 */
public class InsufficientStorageAccessPermissionsException extends StorageException {
	public InsufficientStorageAccessPermissionsException(String userId, String path, String explanation) {
		super("Insufficient permissions for: " + path + "  by " + userId + ": " + explanation + ".");
	}
}
