package gw.storage;

/**
 * The base exception class for the storage system.
 */
public class StorageException extends Exception
{
    /**
     * Constructor
     */
    public StorageException(String message)
    {
        super(message);
    }
    
    public StorageException(Throwable cause)
    {
    	super();
    	initCause(cause);
    }

}
