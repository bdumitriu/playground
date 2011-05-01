package gw.query;

import org.apache.lucene.index.IndexReader;

import java.util.Iterator;
import java.util.Collection;

import java.io.IOException;

import java.util.Map;

/** The IndexStats query implements a query that shows us some statistics about the index.
* These statistics basically are the version of the index, the number of indexed documents
* and also the fields we have indexed. Of course this information is usefull to:
* developers,
* testers
* and system administrators
* Also for users it might be usefull: they can thus know what fields they can search.
*/
public class IndexStatsQuery implements SearchHandler
{
    /** Returns an SearchResult of type IndexStatisticsResult containing the statistics
    * for the current index.
    * @param parameterMap Containing all parameters to the search, not used for the Index Statistics
    * @return SearchResult of type IndexStatisticsResult with the statistics
    */
    public SearchResult search(Map parameterMap) throws IOException
    {
        IndexReader reader = IndexReader.open(Search.indexPath);

        long version = IndexReader.getCurrentVersion(Search.indexPath);
        int numdocs = reader.numDocs();
        Collection col = reader.getFieldNames();
        Iterator iterator = col.iterator();
        return new IndexStatisticsResult(version,numdocs,iterator);
    }
}
