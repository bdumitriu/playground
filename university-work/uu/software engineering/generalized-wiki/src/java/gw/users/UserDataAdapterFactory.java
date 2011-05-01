package gw.users;

import gw.storage.*;

/**
 * The user data adapter factory constructs user data adapters.
 */
public class UserDataAdapterFactory {
    /**
     * Fetches a user data adapter.
     * @param storage A Storage object to initialize the user data adapter with.
     * @return A user data adapter.
     */
    public static UserDataAdapter getUserDataAdapter(Storage storage) {
		UserDataAdapter uda = null;

		try {
			uda = new StorageUserDataAdapter("/Users", storage);
		}
		catch (StorageException sEx) {
			throw new IllegalStateException("failed to get user data adapter");
		}
        
		return uda;
    }
}
