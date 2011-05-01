package gw.query;

import java.io.PrintWriter;
import java.util.Iterator;

/** Search result for an Index Query showing a few nice stastics about the index.
*These statistics are: version, number of indexed documents, and indexed fields.
*/
public class IndexStatisticsResult implements SearchResult {
    private long _version;
    private int _numDocs;
    private Iterator _fields;

    /** Create a new IndexStatisticsResult.
    *@param version The version of the index
    *@param numDocs The number of documents in the index
    *@param fields An iterator with all the fields in the index
    */
    public IndexStatisticsResult (long version, int numDocs, Iterator fields)
    {
        _version = version;
        _numDocs = numDocs;
        _fields = fields;
    }


    /** The result for this query would be the number of indexed documents.
    *@return Number of documents indexed
    */
    public int resultCount()
    {
        return _numDocs;
    }

    /** Show the statistics we gathered before.
    * @param out The printWriter on which we print the statistics.
    */
    public void showResult(PrintWriter out)
    {
            out.println("<p><gw:header level=\"1\">Index Statistics</gw:header></p>");
            out.println("<p><b>Index version: </b>" + _version + "</p>");
            out.println("<p><b>Total number of indexed documents: </b>" + _numDocs + "</p>");
            out.println("<p><b>Indexed fields:</b></p>");
            int i = 0;
            String field = "";
            while(_fields.hasNext())
            {   field = (String)_fields.next();
                out.println("<p>" + (i+1) + "." + field + ".</p>");
                i++;
            }

    }

    /** Since we have no real query we do not want to show anything at all
    * @param out The printwriter to which we would've shown the query.
    */
    public void showQuery(PrintWriter out)
    {

    }



}
