package gw.query;

import java.io.PrintWriter;

/** The SearchResult interface defines the results from a search.
* This interface can be used to pass on these search results.
*/
public interface SearchResult {
    /** Outputs the result in a nice and stylish way to the PrintWriter.
    * @param out The printerWriter on which we want to output the result
    */
    public void showResult(PrintWriter out);

    /** Shows the query that was done for this search. A possible solution would be to
    * output nothing also, it is not always wanted to show a query to the user.
    * @param out The printerWriter on which we want to output the query
    */
    public void showQuery(PrintWriter out);

    /** Count the number of hits in this result.
    * @return The number of hits in this result
    */
    public int resultCount();
}
