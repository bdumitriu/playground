package gw.query;

import java.io.PrintWriter;

import java.util.Vector;

/**
 * Search result for a LinkSearch (query). It is intended to show the results
 * for a dead/live pages query.
 * 
 */
public class LinkSearchResult implements SearchResult {
    private Vector _result;
    private String _type;

    /**
     * Creates a new LinkSearchResult, which can be either the result of a
     * deadpages query or a livepages query. This is set using the type
     * argument.
     * 
     * @param type
     *            The type of the linksearch result. For example: dead for a
     *            dead pages query.
     */
    public LinkSearchResult(String type) {
        _result = new Vector();
        _type = type;
    }

    /**
     * Shows the result for the LinksQuery, which can be either a DeadPages
     * query, or a LivePages query. This type is also regarded in the output.
     * 
     * @param out
     *            The printWriter to which we want to show the result.
     */
    public void showResult(PrintWriter out) {
        if (_result == null || _result.size() == 0)
            out.println("<p><b>No " + _type + " pages in the GW.</b></p>");
        else {
            out.println("<p><gw:header level=1>" + _result.size() + " " + _type
                    + " page(s) found.</gw:header></p>");

            for (int i = 0; i < _result.size(); i++) {
                String path = (String) _result.get(i);
                String link = "/gw/view" + path;
                out.println("<p>" + (i + 1) + ". <gw:link href=\"" + link + "\">" + path
                        + "</gw:link></p>");
            }
        }
    }

    /**
     * The result count is the number of dead/live/other pages in we added to
     * this result.
     * 
     * @return Number of pages found using linksearch.
     */
    public int resultCount() {
        return _result.size();
    }

    /**
     * Adds a file to the result of the query, which will be shown using
     * showResult.
     * 
     * @param file
     *            The file that is a result of the query
     */
    public void add(String file) {
        _result.add(file);
    }

    /**
     * Since LinkSearch query is not supposed to be directly shown to the user,
     * no query needs to be specified in here.
     * 
     * @param out
     *            The printwriter to which we would've shown the query.
     */
    public void showQuery(PrintWriter out) {

    }

}
