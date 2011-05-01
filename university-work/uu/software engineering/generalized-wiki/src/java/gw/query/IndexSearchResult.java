package gw.query;

import java.io.PrintWriter;
import java.io.IOException;

import org.apache.lucene.search.Hits;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.DateField;

import java.text.DateFormat;
import java.util.*;

/**
 * The result from a search on the index.
 */
public class IndexSearchResult implements SearchResult {
    private Hits _resultHits;
    private String _searchType, _searchQuery;

    /**
     * Sets the hits as the result for the search.
     * 
     * @param result
     *            The result for this search
     */
    public IndexSearchResult(Hits result) {
        _resultHits = result;
    }

    /**
     * Shows the number of results
     * 
     * @return the number of hits in this result
     */
    public int resultCount() {
        if (_resultHits == null) {
            return 0;
        } else {
            return _resultHits.length();
        }
    }

    /**
     * Return the String[] containing all the paths to the hits from this
     * result. This can be used if you just did a query and are only interested
     * in the files it hit.
     * 
     * @return A String[] containing all the filepaths to the hits returned by
     *         the search.
     * @return An empty String[] with size 0 when there is no result
     * @throws IOException
     *             when the index returns an IOException while getting a file.
     */
    public String[] getHitPaths() throws IOException {
        String[] paths = new String[_resultHits.length()];
        for (int i = 0; i < _resultHits.length(); i++) {
            Document doc = _resultHits.doc(i);
            paths[i] = doc.get("path");

        }
        return paths;
    }

    /**
     * sets the Searhc type and Query for this search result. Since it is an
     * index search result, the search type just denotes the fields in which the
     * basic search started. The query is the query we ran on the index.
     * 
     * @param type
     *            The field in the index to look in.
     * @param query
     *            The query we ran on the index.
     */
    public void setSearchTypeAndQuery(String type, String query) {
        _searchType = type;
        _searchQuery = query;
    }

    /**
     * We show here a new form which allows us to do another index search on the
     * same field. We also show the query again which the user specified before.
     * 
     * @param out
     *            The PrintWriter to which we print the information.
     */
    public void showQuery(PrintWriter out) {
        if (_searchType != null && _searchQuery != null) {
            out.println("<p><b>Search:</b><form method=post action=/gw/search/>"
                    + "<input type=text name=\"query\" value=\"" + _searchQuery
                    + "\"><br><input type=hidden name=\"type\" value=\"" + _searchType
                    + "\"> <input type=\"submit\"  value=\"Search\"></form></p>");

        }
    }

    /**
     * Shows the result for this indexsearch result.
     * 
     * @param out
     *            The PrintWriter on which we want to show the result.
     */
    public void showResult(PrintWriter out) {
        if (_resultHits == null || _resultHits.length() == 0) {
            out.println("<p>No matching document found.</p>");
        } else {
            out.println("<p><gw:header level=1>" + _resultHits.length()
                    + " document(s) found.</gw:header></p>\n");
            out.flush();
            for (int i = 0; i < _resultHits.length(); i++) {
                try {
                    Document doc = _resultHits.doc(i);
                    String path = doc.get("path");
                    String link = "/gw/view" + path;
                    out.println("<p>" + (i + 1) + ". <gw:link href=\"" + link + "\">" + path
                            + "</gw:link></p>");
                    String modified;
                    try {
                        Date modifiedDate = DateField.stringToDate(doc.get("modified"));
                        modified = DateFormat.getInstance().format(modifiedDate);
                    } catch (Exception e) {
                        modified = Calendar.getInstance().getTime().toString();
                    }
                    out.println("<p>Last modified: " + modified + "</p>");
                } catch (Exception e) {
                    String message = "<p><b>While showing the result for your search, the "
                            + (i + 1) + "th result returned an error</b></p>";
                    out.println(message);
                    e.printStackTrace(out);
                }
            }
        }
    }
}
