package gw.storage;

/**
 * The AuthStorageException is thrown whenever something goes
 * wrong in the authentication process
 */
public class AuthStorageException extends StorageException
{
    public AuthStorageException(String message)
    {
        super(message);
    }
};