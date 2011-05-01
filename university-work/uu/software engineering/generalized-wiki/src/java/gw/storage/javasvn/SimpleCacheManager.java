package gw.storage.javasvn;

import java.util.HashMap;
import java.io.*;


/** A simple cache.
 * example of a more advanced caching mechanism: http://www.complex-ite.net/tutorials/JDCBook/perf4.html
 *
 * There is no way to manage things that are inside the cache.
 * No support for deleting files from the cache when there is a time-out
 * or something.
 *
 */
public class SimpleCacheManager
{
    private static SimpleCacheManager _manager = new SimpleCacheManager();
    private HashMap _cache;

    /**
     * Constructor. 
     */
    public SimpleCacheManager()
    {
        _cache = new HashMap();
    }

    public HashMap getCache()
    {
        return _cache;
    }

    public static synchronized boolean fileExists(String path, long revision)
    {
        HashMap map = _manager.getCache();
        return map.containsKey(hashPath(path, revision));
    }

    public static InputStream getFile(String path, long revision) throws FileNotFoundException
    {
        HashMap map = _manager.getCache();
        //System.out.println("Getting file from cache: '" +path +"' (rev:"+revision+")");
        path = hashPath(path, revision);
        File file = (File)map.get(path);
        return new BufferedInputStream(new FileInputStream(file));
    }

    // returns the path of the tempfile
    public static String getTempFile(String path, long revision) throws FileNotFoundException
    {
        HashMap map = _manager.getCache();
        path = hashPath(path, revision);
        File file = (File)map.get(path);
        return file.getPath();
    }
    
    public static synchronized void addFile(String path, long revision, byte[] contents) throws IOException
    {
        HashMap map = _manager.getCache();
        path = hashPath(path, revision);
        File file = File.createTempFile("RACache", null);
        OutputStream fos = new BufferedOutputStream(new FileOutputStream(file));
        fos.write(contents);
        fos.close();
        map.put(path, file);
    }

    private static String hashPath(String path, long revision)
    {
        return path + String.valueOf(revision);
    }
}
