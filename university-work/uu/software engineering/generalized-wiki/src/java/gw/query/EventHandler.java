package gw.query;

import gw.storage.*;

/** Allows us to listen for Storage events. This basically allows us to
* do incremental updates. When we receive and event we can just call the IndexUpdater.
*/
public class EventHandler implements StorageListener {

    /** A file is added to the wiki, so we want to index it.
    * @param storage The storage on which it was added
    * @param path The path of the new file.
    */
    public void fileAdded( Storage storage, String path ) {
        try {
            IndexUpdater updater = new IndexUpdater( storage );
            updater.addFile( path );
        } catch( Exception e ) {}
    }

     /** A file is deleted from the wiki, so we want remove it from the index.
    * @param storage The storage on which it was deleted
    * @param path The path of the (now removed) file.
    */
    public void fileDeleted( Storage storage, String path ) {
        try {
            IndexUpdater updater = new IndexUpdater( storage );
            updater.deleteFile( path );
        } catch( Exception e ) {}
    }

     /** A file is modified on the wiki, so we want to re-index it.
    * @param storage The storage on which it was modified
    * @param path The path of the modified file.
    */
    public void fileModified( Storage storage, String path ) {
        try {
            IndexUpdater updater = new IndexUpdater( storage );
            updater.updateFile( path );
        } catch( Exception e ) {}
    }

     /** A file is moved on the Wiki. What we just do is remove it from the index
     * and then add it again to the index using its new path.
    * @param storage The storage on which it was move
    * @param from Original location of the file
    * @param to New path of the file.
    */
    public void fileMoved( Storage storage, String from, String to ) {
        fileDeleted( storage, from );
        fileAdded( storage, to );
    }


}
