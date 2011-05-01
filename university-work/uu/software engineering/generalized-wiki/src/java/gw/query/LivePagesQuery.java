package gw.query;

import org.apache.lucene.search.*;

import java.io.IOException;
import java.util.Map;

/**
 * This class handles a query for live pages. A live page is a page with at
 * least 1 links to it.
 */
public class LivePagesQuery implements SearchHandler {
    private IndexSearch _searcher = new IndexSearch();

    /**
     * Returns a LinkSearch result with all the live pages in it. Of course the
     * input is not nessecary for this type of search, but we need it anyway for
     * the SearchHandler interface. We define being live as being not dead.
     * 
     * @param parameterMap
     *            Containing all parameters to the search, not used for
     *            LivePages
     * @return A LinkSearchResult with all the dead pages in the wiki
     */
    public SearchResult search(Map parameterMap) {
        LinkSearchResult result = new LinkSearchResult("live");
        DeadPagesQuery deadpages = new DeadPagesQuery();

        try {
            IndexSearcher searcher = new IndexSearcher(Search.indexPath);
            for (int i = 0; i < searcher.maxDoc(); i++) {
                try {
                    String path = searcher.doc(i).get("path");
                    if (!deadpages.isDead(path)) {
                        result.add(path);
                    }
                } catch (IllegalArgumentException e) {
                    // File was deleted from the index, lets move on to the next
                    // file.
                }

            }
        } catch (IOException e) {
        }

        return result;
    }

    /**
     * Finds out wether a certain file is dead or not
     * 
     * @param file
     *            File to check
     * @return true when other files link to this file.
     */
    public boolean isLive(String file) {
        try {
            SearchResult result = _searcher.searchIndex("links", file);

            if (result.resultCount() > 0) {
                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            // Obviously something went wrong searching for the file.
            // So we must assume the file is live...
            return true;
        }
    }

}
