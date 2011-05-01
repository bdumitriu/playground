package gw.query;

import java.io.PrintWriter;

/**
 * There was some error while we tried to do a search. This error result is able
 * to show this error to the user. Please note: it is not always wanted to show
 * this error!
 */
public class ErrorResult implements SearchResult {
    private String _errorMessage;
    private String _searchType;
    private Exception _exception;

    /**
     * We have some error string why we have no decent result.
     * 
     * @param message
     *            The error message for the error we got when trying to do the
     *            search.
     */
    public ErrorResult(String message) {
        _errorMessage = message;
        _searchType = "Search error";

    }

    /**
     * For a search type we have some error message.
     * 
     * @param type
     *            Search type in which the error occured.
     * @param message
     *            The error message for the error we got when trying to do the
     *            search.
     */
    public ErrorResult(String type, String message) {
        _searchType = type;
        _errorMessage = message;

    }

    /**
     * For a search type we have some error message.
     * 
     * @param type
     *            Search type in which the error occured.
     * @param message
     *            The error message for the error we got when trying to do the
     *            search.
     * @param exception
     *            The error exception we got
     */
    public ErrorResult(String type, String message, Exception exception) {
        _searchType = type;
        _errorMessage = message;
        _exception = exception;

    }

    /**
     * Since we have an error, we always have 0 results.
     * 
     * @return int with value 0
     */
    public int resultCount() {
        return 0;
    }

    /**
     * Shows the result of the query. In this case it thus is the error we got.
     * Nice thing is we can print a complete stacktrace. Usefull for debugging,
     * but might not be wanted in a live GW.
     * 
     * @param out
     *            PrintWriter to write to.
     */
    public void showResult(PrintWriter out) {
        out.println("<p><gw:header level=1>" + _searchType + "</gw:header></p>");
        if (_errorMessage != null) {
            out.println(_errorMessage);
        }
        if (_exception != null) {
            out.println("<gw:verbatim>");
            _exception.printStackTrace(out);
            out.println("</gw:verbatim>");
        }

    }

    /**
     * Since we have no query, but an error, we do not want to show anything at
     * all
     * 
     * @param out
     *            The printwriter to which we would've shown the query.
     */
    public void showQuery(PrintWriter out) {

    }

    /**
     * Returns the exception set in this error result.
     * 
     * @return the exception set for this error result.
     */
    public Exception getException() {
        return _exception;
    }

}
