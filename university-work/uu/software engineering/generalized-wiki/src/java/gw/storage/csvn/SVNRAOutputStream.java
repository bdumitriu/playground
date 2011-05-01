package gw.storage.csvn;

import java.io.*;

/**
 * A helper subclass of FileOutputStream to auto-add new files to the Subversion repository
 */
public class SVNRAOutputStream extends FileOutputStream
{
    private SVNRAStorage _storage;
    private File _file;
    private String _path;
    private boolean _exists;
    private boolean _closed = false;
    
    public SVNRAOutputStream(SVNRAStorage storage, File file, String path, boolean exists) throws IOException
    {
        super(file);
        _storage = storage;
        _file = file;
        _path = path;
        _exists = exists;
    }

    public void close() throws IOException
    {
        if (!_closed)
        {
            _closed = true;
            try
            {
                SVNRAAction action = new SVNRAAction();
                if (_exists) {
                    action.type = SVNRAAction.MODIFY;
                } else {
                    action.type = SVNRAAction.ADD;
                }
                action.path1 = _path;
                action.file = _file;
                _storage._actions.add(action);
            }
            catch (Exception e)
            {
                throw new IOException(e.getMessage());
            }
        }

        super.close();
    }
}
