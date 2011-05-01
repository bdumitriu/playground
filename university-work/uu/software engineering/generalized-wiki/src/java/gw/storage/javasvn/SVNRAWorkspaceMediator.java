package gw.storage.javasvn;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.ISVNWorkspaceMediator;

/**
 * A custom implementation of ISVNWorkspaceMediator.
 * 
 * @author Bogdan Dumitriu
 */
public class SVNRAWorkspaceMediator implements ISVNWorkspaceMediator
{
    private Map<Object, ByteArrayOutputStream> tempLocations;

    public SVNRAWorkspaceMediator()
    {
        tempLocations = new HashMap<Object, ByteArrayOutputStream>();
    }

    /**
     * @see org.tmatesoft.svn.core.io.ISVNWorkspaceMediator#createTemporaryLocation(java.lang.String, java.lang.Object)
     */
    public OutputStream createTemporaryLocation(String path, Object id) throws IOException
    {
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        tempLocations.put(id, stream);

        return stream;
    }

    /**
     * @see org.tmatesoft.svn.core.io.ISVNWorkspaceMediator#deleteTemporaryLocation(java.lang.Object)
     */
    public void deleteTemporaryLocation(Object id)
    {
        tempLocations.remove(id);
    }

    /**
     * @see org.tmatesoft.svn.core.io.ISVNWorkspaceMediator#getLength(java.lang.Object)
     */
    public long getLength(Object id) throws IOException
    {
        ByteArrayOutputStream os = tempLocations.get(id);
        if (os != null)
        {
            return os.size();
        }
        else
        {
            return 0;
        }
    }

    /**
     * @see org.tmatesoft.svn.core.io.ISVNWorkspaceMediator#getTemporaryLocation(java.lang.Object)
     */
    public InputStream getTemporaryLocation(Object id) throws IOException
    {
        return new ByteArrayInputStream(tempLocations.get(id).toByteArray());
    }

    /**
     * @see org.tmatesoft.svn.core.io.ISVNWorkspaceMediator#getWorkspaceProperty(java.lang.String, java.lang.String)
     */
    public String getWorkspaceProperty(String path, String value) throws SVNException
    {
        return null;
    }

    /**
     * @see org.tmatesoft.svn.core.io.ISVNWorkspaceMediator#setWorkspaceProperty(java.lang.String, java.lang.String, java.lang.String)
     */
    public void setWorkspaceProperty(String path, String name, String value) throws SVNException
    {}
}
