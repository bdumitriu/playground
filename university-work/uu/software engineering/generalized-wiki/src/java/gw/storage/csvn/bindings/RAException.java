package gw.storage.csvn.bindings;

/**
 * The RAException is thrown whenever something goes
 * wrong during access to the repository layer .
 */
public class RAException extends Exception
{
    /* error codes */
    public final static int MERGE_CONFLICT              = 160024;
    public final static int UNSUP_REPOS_VERSION         = 165005;
    public final static int BAD_PROPERTY_NAME           = 195011;

    /* RA errors */
    public final static int BAD_URL_TO_RA_LAYER         = 170000;
    public final static int AUTHORIZATION_FAILED        = 170001;
    public final static int UNKNOWN_AUTH_METHOD         = 170002;
    public final static int RA_METHOD_NOT_IMPL          = 170003;
    public final static int ITEM_OUT_OF_DATE            = 170004;
    public final static int REPOS_NOT_FOUND             = 180000;
    public final static int COULD_NOT_OPEN_REPOS        = 180001;

    /* libsvn_auth errors */
    public final static int CREDENTIAL_DATA_UNAVAILABLE = 215000;
    public final static int NO_AUTH_PROVIDER_AVAILABLE  = 215001;
    public final static int ALL_AUTH_PROVIDER_EXHAUSTED = 215002;

    // svn error code
    private int _error;


    /**
     * Constructor for creating a RA exeption.
     *
     * @param message Message for the exception message
     * @param error RA layer error code
     */
    public RAException(String message, int error)
    {
        super(message);
        _error = error;
    }

    /**
     * Get the RA layer error code.
     *
     * @return RA error code
     */
    public int getErrorCode()
    {
        return _error;
    }
}
