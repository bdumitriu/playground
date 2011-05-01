package gw.storage;

import java.util.Iterator;
import java.util.List;

/**
 * @author John van Schie
 */
public abstract class AbstractStorage implements Storage {
	
	/** 
	 * Helper method to fire a fileAdded event
	 *
	 * @param path the name of the new file
	 */
	protected void fireFileAdded(String path)
	{
		List listeners = StorageListenerContainer.getListeners();
        
		Iterator iterator = listeners.iterator();
        
		while (iterator.hasNext())
		{
			StorageListener listener = (StorageListener) iterator.next();
            
			listener.fileAdded(this, path);
		}
	}

	/**
	 * Helper method to fire a fileDeleted event
	 *
	 * @param path the name of the deleted file
	 */
	protected void fireFileDeleted(String path)
	{
		List listeners = StorageListenerContainer.getListeners();
        
		Iterator iterator = listeners.iterator();
        
		while (iterator.hasNext())
		{
			StorageListener listener = (StorageListener) iterator.next();
            
			listener.fileDeleted(this, path);
		}
	}
    
	/**
	 * Helper method to fire a fileModified event
	 *
	 * @param path the name of the modified file
	 */
	protected void fireFileModified(String path)
	{
		List listeners = StorageListenerContainer.getListeners();
        
		Iterator iterator = listeners.iterator();
        
		while (iterator.hasNext())
		{
			StorageListener listener = (StorageListener) iterator.next();
            
			listener.fileModified(this, path);
		}
	}

	/**
	 * Helper method to fire a fileMoved event
	 *
	 * @param from the original name of the file
	 * @param to the new name of the file
	 */
	protected void fireFileMoved(String from, String to)
	{
		List listeners = StorageListenerContainer.getListeners();
        
		Iterator iterator = listeners.iterator();
        
		while (iterator.hasNext())
		{
			StorageListener listener = (StorageListener) iterator.next();
            
			listener.fileMoved(this, from, to);
		}
	}
	
	public void ensurePathExists(String path) throws StorageException {
   	 	String[] newDirs = path.split("/");
        String pathToCreate = "";

        for (int j = 0; j < newDirs.length; j++) {
            pathToCreate += "/" + newDirs[j];

            if (!fileExists(pathToCreate)) {
                makeDirectory(pathToCreate);
            }
        }
   }
}
