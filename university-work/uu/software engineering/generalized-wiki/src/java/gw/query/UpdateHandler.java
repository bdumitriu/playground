package gw.query;

import gw.storage.*;

import java.io.*;
import org.apache.lucene.document.Document;

/** An UpdateHandler adds fields to the Document specified for a certain file.
* This is used to get all information for a search method into the index.
* This allows us to store all kind of fields into the index without
* extensivly modifying the index builder.
*/
public interface UpdateHandler
{
    /** updates the document with some field. The document to update should be
    * given by the index updater.
    * @param storage The storage from which we can read the file.
    * @param file The file to read the information from.
    * @param isDir Whether this file is really a file or a directory
    * @param doc The document to which we write the field.
    */
    public void update(Storage storage, String file, boolean isDir, Document doc) throws IOException;
}
