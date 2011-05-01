package gw.storage;

/**
 * The listener interface for receiving storage events.
 */
public interface StorageListener {
    /**
     * Invoked when a file is added to the storage
     * 
     * @param source
     *            the Storage object that fired the event
     * @param path
     *            the file
     */
    public void fileAdded(Storage source, String path);

    /**
     * Invoked when a file is deleted from the storage
     * 
     * @param source
     *            the Storage object that fired the event
     * @param path
     *            the file
     */
    public void fileDeleted(Storage source, String path);

    /**
     * Invoked when a file (or a property of a file) is modified
     * 
     * @param source
     *            the Storage object that fired the event
     * @param path
     *            the file
     */
    public void fileModified(Storage source, String path);

    /**
     * Invoked when a file is moved (or renamed)
     * 
     * @param source
     *            the Storage object that fired the event
     * @param from
     *            the old file
     * @param to
     *            the new file
     */
    public void fileMoved(Storage source, String from, String to);
}
