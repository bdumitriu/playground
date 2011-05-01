package gw.query;

import gw.storage.*;
import java.io.*;
import org.apache.lucene.document.*;

/**
 * LinkSearch provides us with a Links field in the index. We can now search for
 * the links a certain file contains. This Links field in the index can also be
 * used for querying the web structure. Also, LinkSearch provides us with
 * external functionality: query for all files that contain a link to file X.
 */
public class LinkSearch implements UpdateHandler {

    /**
     * Update the index with a links field, containing all links that are in the
     * file. So that are url's, but also WikiNames.
     * 
     * @param storage
     *            The storage from which we can read the file.
     * @param file
     *            The file to read the links from.
     * @param isDir
     *            Whether this file is really a file or a directory
     * @param doc
     *            The document to which we write the links field.
     */
    public void update(Storage storage, String file, boolean isDir, Document doc) {
        if (!isDir) {
            PageInfo info = new PageInfo(storage, file);
            String links[] = info.getLinks();
            String all_links = "";
            for (int i = 0; links != null && i < links.length; i++) {
                all_links = all_links + links[i] + " ";
            }

            doc.add(Field.Text("links", all_links));

        }
    }

    /**
     * Provides all files that link to the input file.
     * 
     * @param path
     *            The path of the file that you want the other files linking to
     * @return A string array containing all filepaths linking to this file.
     * @throws IOException
     *             when there was an error while reading from the index
     * @throws Exception
     *             when one has occured deeper down the search methods.
     */
    public String[] getLinkingFiles(String path) throws Exception, IOException {
        IndexSearch searcher = new IndexSearch();
        SearchResult result = searcher.searchIndex("links", path);
        if (result instanceof IndexSearchResult) {
            IndexSearchResult searchresult = (IndexSearchResult) result;

            return searchresult.getHitPaths();
        } else {
            ErrorResult error = (ErrorResult) result;
            throw error.getException();
        }
    }
}
