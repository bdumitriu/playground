package gw;


import javax.servlet.http.HttpSession;
import javax.servlet.ServletContext;

import gw.storage.SecuredStorage;
import gw.storage.SessionStorageFactory;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.users.SessionAuthenticationManager;
import gw.users.SessionAuthenticationManagerFactory;
import gw.users.User;

import java.util.Map;

import java.io.Serializable;

/**
 * The SessionContext of GW provides access to session-specific components
 * and resources.
 * It is normally stored in a HttpSession instance for easy access from HttpRequests.
 * 
 * TODO: unit tests.
 * 
 * @author Damir Alagic
 */
public class GwSessionContext implements Serializable {
	
	private static final long serialVersionUID = 6845275319863245736L;
	
    /**
     * Constant that gives the key of the session context.
     */
    public static final String SESSION_CONTEXT  = "gw.sessioncontext";
    
    // Session storage (secure).
    private Storage _sessionStorage;
    
    // Unsecured session storage, needed for some admin operations.
    // TODO: maybe make this static and move it to elsewhere?
    private Storage _unsecuredStorage;
    
    // The user that owns this session.
    private User _owner;
    
    // GwContext, for configuration resources
    private GwContext _gwContext;

    // Parameters of this session.
    private Map _parameters;
    
    // Factories
    private SessionAuthenticationManagerFactory _samFactory;
    
    // A session authentication manager for use with this session.
    private SessionAuthenticationManager _sam;
    
    /**
     * Main constructor, requires a global config-object for instantiating storages.
     * 
     * @param config The global context configuration, needed to create session storages.
     */
    public GwSessionContext(GwContext gc) {
        _gwContext = gc;
        initUnsecureStorage();
        _samFactory = new SessionAuthenticationManagerFactory(this);
        _owner = User.ANONYMOUS_USER;
        initSecureStorage();
    }
    
    /**
     * Returns the session's storage.
     * @return The current session's Storage.
     */
    public Storage getSessionStorage() {
        return _sessionStorage;
    }
    
    public void setSessionStorage(Storage sessionStorage) {
        this._sessionStorage = sessionStorage;
    }

    /**
     * Returns an unsecured Storage.
     * @return An unsecured Storage. 
     */
    public Storage getUnsecuredStorage() {
        return _unsecuredStorage;
    }
    
    /**
     * Returns the owner of this session (or null if there was no login);
     * @return The User object that owns this session.
     */
    public User getOwner() {
        return _owner;
    }
    
    /**
     * Sets the owner of this session and initializes the secured storage.
     * This is called by the Session Auth Manager upon succesful login.
     * 
     * @param owner The User object that owns this session.
     */
    public void setOwner(User owner) {
        _owner = owner;
        initSecureStorage();
    }
    
    /**
     * Returns the parameters of this session.
     * @return The parameters of this session.
     */
    public Map getParameters() {
        return _parameters;
    }
    
    /**
     * Set the paramaters of this session.
     * @param parameters The new parameters.
     */
    public void setParameters(Map parameters) {
        _parameters = parameters;
    }
        
    /**
     * Get the context from a session.
     * 
     * @param session The session.
     * @return The GwSessionContext object (or null if it doesn't exist yet).
     */
    public static GwSessionContext getContextFromSession(HttpSession session) {
        if (session == null)
            throw new IllegalArgumentException("session = null");
        
        GwSessionContext context = (GwSessionContext) session.getAttribute(SESSION_CONTEXT);
        return context;
    }
    
    /**
     * Puts context objects onto a session.
     * @param session The session.
     * @param config A global configuration object, used for creating session storages.
     */
    public static void installContextOnSession(HttpSession session, GwContext gc) {
        if (session == null)
            throw new IllegalArgumentException("session = null");
        if (gc == null)
            throw new IllegalArgumentException("config = null");
        
        if (session.getAttribute(SESSION_CONTEXT) != null)
            throw new IllegalStateException("There is already a context on the session.");
        
        session.setAttribute(SESSION_CONTEXT, new GwSessionContext(gc));
    }

    /**
     * Initializes the unsecure storage.
     * 
     * @param config Global configuration.
     */
    private void initUnsecureStorage() {
        String username = _gwContext.getServletContext().getInitParameter(GwConstants.SVN_USER);
        String password = _gwContext.getServletContext().getInitParameter(GwConstants.SVN_PASSWORD);
        String repository = _gwContext.getServletContext().getInitParameter(GwConstants.SVN_REPOSITORY);
        
        Storage unsecuredStorage = null;
        try {
            unsecuredStorage = SessionStorageFactory.getSVNStorage(username, password, repository);
            _unsecuredStorage = unsecuredStorage;
        }
        catch(StorageException se) {
            System.err.println(se.getMessage());
        }
        
        if (unsecuredStorage == null)
            throw new NullPointerException("unsecured storage not initialized.");
    }
    
    /**
     * Initializes the secure storage (Authenticates the user). Should be called with
     * an existing unsecureStorage.
     * 
     * @param config Global configuration.
     */
    private void initSecureStorage() {
        Storage securedStorage = null;
        
        securedStorage = new SecuredStorage(_unsecuredStorage, _owner);
        
        if (securedStorage == null)
            throw new NullPointerException("Secured storage not initialized.");
        
        _sessionStorage = securedStorage;
    }
    
    /**
     * Fetches the Session Auth Manager of this context.
     * Creates a new one if null.
     * 
     * @return The Session Auth Manager.
     */
    public SessionAuthenticationManager getSam() {
    	if(_sam == null)
    		_sam = (SessionAuthenticationManager) _samFactory.getAuthenticationManager(_unsecuredStorage);

        return _sam;
    }
}
