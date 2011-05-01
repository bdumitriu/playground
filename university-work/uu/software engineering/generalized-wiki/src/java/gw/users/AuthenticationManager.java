package gw.users;

import java.util.Map;

/**
 * The authentication manager authenticates users. It is an entry-point
 * for authentication of users.
 */
public interface AuthenticationManager {
    /**
     * Attempts to login the user.
     * 
     * @param userId User to be logged in. 
     * @param authParameters Map with implementation-specific parameters for authentication
     */
    public boolean login( final String userId,  final Map authParameters );

    /**
     * Logs out the user.
     */
    public void logout();
}
