package gw.storage.javahl;

import gw.storage.*;

/**
 * The SVNStorageException is thrown whenever something goes
 * horribly wrong in a subversion call
 */
public class SVNStorageException extends StorageException
{
    public SVNStorageException(String message)
    {
        super(message);
    }
};