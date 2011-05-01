package gw.users;


import java.util.Map;
import gw.GwSessionContext;
import gw.util.DigestAuthenticationUtility;


/**
 * Authentication from a session. After a login, we store the User in the
 * session context.
 * 
 * The authentication manager is an entry point for authorization and authentication.
 */
public class SessionAuthenticationManager implements AuthenticationManager {    
    private GwSessionContext    _sessionContext;
    private UserDataAdapter     _uda;
    
    // Authentication challenge storage.
    private String _firstCNonce;
    private String _firstNonce;
    
    /**
     * Constructs a SessionAuthenticationManager.
     *
     * @param sessionContext Session context to store user information.
     * @param uda A properly initialized UserDataAdapter
     */
    public SessionAuthenticationManager( GwSessionContext sessionContext, final UserDataAdapter uda ) {
    	_uda	        = uda;
        _sessionContext = sessionContext;
        _firstCNonce    = null;
        _firstNonce     = null;
    }

    /**
     * Logins the user. If it succeeds (returns true) the user is added to the session.
     * 
     * @param user The (String) name of the client
     * @param authParameters Map with parameters for session authentication
     */
    public boolean login( final String user, final Map authParameters ) {
        String userId        = (String) authParameters.get("username");
    	String qop           = (String) authParameters.get("qop");
        String nonce         = (String) authParameters.get("nonce");
        String nc            = (String) authParameters.get("nc");
        String cnonce        = (String) authParameters.get("cnonce");
        String uri           = (String) authParameters.get("uri");
        String algorithm     = (String) authParameters.get("algorithm");
        String requestMethod = (String) authParameters.get("requestMethod");
        
        if( !user.equals(userId) )
            return false;
        if( nonce == null || getNonce() == null)
            return false;
        if( !nonce.equals( getNonce() ) )
            return false;
        if( getCNonce() == null )
            setCNonce( cnonce );
        
        if (!_uda.exists(new User(userId)))
            return false;
        
        String passwordHash = getPasswordHash(userId, algorithm, _firstNonce, _firstCNonce);
        String resultHash = null;

        if (qop != null && qop.equals("auth")) {
            if (nc == null || cnonce == null)
                throw new IllegalArgumentException("No nc or cnonce value in the authentication request.");
            
        resultHash = DigestAuthenticationUtility.getResultHash( passwordHash
                                                              , nonce
                                                              , nc
                                                              , cnonce
                                                              , qop
                                                              , requestMethod
                                                              , uri
                                                              );
        } else {
            resultHash = DigestAuthenticationUtility.getResultHash( passwordHash
                                                                  , nonce
                                                                  , requestMethod
                                                                  , uri
                                                                  );
        }

        final boolean passwordOk = ((String)authParameters.get("response")).equals( resultHash );
        
        if( passwordOk ) {
        	User loggedInUser = new User(userId);
            _sessionContext.setOwner( loggedInUser );
            _uda.load( loggedInUser );
        }

        return passwordOk;
    }    
    
    /**
     * Gets the password hash depending on the algorithm.
     * @return The hash.
     */
    private String getPasswordHash(String userId, String algorithm, String firstNonce, String firstCnonce) {
        String passwordHash = _uda.getPassword(new User(userId));
        
        if (algorithm.equals("MD5-sess"))
            return DigestAuthenticationUtility.getSessPasswordHash(passwordHash, firstNonce, firstCnonce);
        else if (algorithm.equals("MD5"))
            return passwordHash;
        else
            throw new IllegalArgumentException("Unsupported authentication algorithm: " + algorithm);
    }

    /**
     * Logouts a user, returning the active user to the anonymous user.
     *
     * @see AuthenticationManager#logout()
     */
    public void logout() {
        _sessionContext.setOwner( User.ANONYMOUS_USER );
    }

    public String getCNonce() {
        return _firstCNonce;
    }
    
    public String getNonce() {
        return _firstNonce;
    }
    
    public void setCNonce(String cn) {
        _firstCNonce = cn;
    }
    
    public void setNonce(String n) {
        _firstNonce = n;
    }
}
