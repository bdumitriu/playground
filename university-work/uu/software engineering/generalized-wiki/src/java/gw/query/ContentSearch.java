package gw.query;

import gw.storage.*;

import java.io.*;
import org.apache.lucene.document.*;

/** ContentSearch provides the contents field into the index.
* Basically the real search later gets handled by the IndexSearch,
* so this class will only implement the updatehandler so it is able
* to create this most important index field.
* @see gw.query.IndexSearch
*/
public class ContentSearch implements UpdateHandler
{
    /** Adds the content field to the given document, using the contents from the file
    * @param storage The storage used to read from the file
    * @param file The file we want to read the contents from
    * @param isDir Whether it is really a file or actually a directory
    * @param doc The document we add the content field to
    */
    public void update(Storage storage, String file, boolean isDir, Document doc) throws IOException
    {
        if(!isDir)
        {
            try {
                InputStream is = storage.getFile( file );
                doc.add(Field.Text("content", new InputStreamReader(is)));
            } catch( Exception e ) {}
        }
    }
}
