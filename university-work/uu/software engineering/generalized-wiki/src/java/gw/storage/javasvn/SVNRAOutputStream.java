package gw.storage.javasvn;

import java.io.*;

/**
 * This class makes sure that users of the SVNRAStorage storeFile method trigger
 * the necessary changes in working copy when they decide to close the file they were
 * editing.
 *
 * @author Patrick Cloosterman
 * @author Bogdan Dumitriu 
 */
public class SVNRAOutputStream extends FileOutputStream
{
    private SVNRAStorage _storage;
    private File _file;
    private String _path;
    private File _baseFile;
    private boolean _closed;
    
    public SVNRAOutputStream(SVNRAStorage storage, File file, String path, File baseFile)
        throws IOException
    {
        super(file);

        _storage = storage;
        _file = file;
        _path = path;
        _baseFile = baseFile;
        _closed = false;
    }

    public void close() throws IOException
    {
        super.close();

        if (!_closed)
        {
            try
            {
                _storage.getWorkingCopy().registerStoreFile(_path, _file, _baseFile);
            }
            catch (Exception e)
            {
                _closed = true;
                throw new IOException(e.getMessage());
            }
            _closed = true;
        }
    }
}
