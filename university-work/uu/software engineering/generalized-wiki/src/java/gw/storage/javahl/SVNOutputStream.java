package gw.storage.javahl;

import java.io.*;
import gw.storage.*;
import org.tigris.subversion.javahl.*;


/**
 * A helper subclass of FileOutputStream to auto-add new files to the Subversion repository.
 */
public class SVNOutputStream extends FileOutputStream
{
    private SVNStorage _storage;
    private File _file;
    private boolean _closed = false;

    /**
     * Constructor.
     */    
    public SVNOutputStream(SVNStorage storage, File file) throws IOException
    {
        super(file);
        _storage = storage;
        _file = file;
    }

    /**
     * @see FileOutputStream#close
     */
    public void close() throws IOException
    {
        if (!_closed)
        {
            _closed = true;
            try
            {
                // Add the file to the repository, if neccesary
                Status[] status = _storage._client.status(_file.getAbsolutePath(), false,
                    false, true);
		    
		if (status.length <= 0)
		    throw new StorageException("Unable to get status for file: \"" + _file.getAbsolutePath() + "\".");

                if (!status[0].isManaged())
                {
                    _storage._client.add(_file.getAbsolutePath(), false);
                }
               
            }
            catch (Exception e)
            {
                throw new IOException(e.getMessage());
            }
        }

        super.close();
    }
}
