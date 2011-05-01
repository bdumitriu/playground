package gw.query;

import java.io.*;
import org.apache.lucene.document.*;
import org.apache.lucene.search.*;
import org.apache.lucene.index.*;
import gw.storage.*;
import java.text.DateFormat;
import java.util.Date;
import java.util.Calendar;
import java.util.Locale;
import java.util.Map;

public class ModifiedSearch implements UpdateHandler, SearchHandler {
    /**
     * We update the document with a field denoting the last time this file was
     * modified.
     */
    public void update(Storage storage, String file, boolean isDir, Document doc) {
        try {
            PageInfo info = new PageInfo(storage, file);
            Date modified = info.getModifiedDate();
            doc.add(Field.Keyword("modified", DateField.dateToString(modified)));
        } catch (Exception e) {
            System.out.println("Failed to update modified date: " + e);
        }
    }

    /**
     * Searches for all files that have been modified after a given date
     * 
     * @param parameterMap
     *            Containing all parameters to the search (in this case it are
     *            dates)
     * @return SearchResult for this modified search.
     */
    public SearchResult search(Map parameterMap) {
        Date from;
        String fromString = SearchUtil.getParameter(parameterMap, "query");
        try {
            from = DateFormat.getDateInstance(DateFormat.SHORT, Locale.US).parse(fromString);
        } catch (Exception e) {

            return new ErrorResult("Modified Search", "Invalid date format");
        }
        try {
            Date now = Calendar.getInstance().getTime();
            Query query;
            query = new RangeQuery(new Term("modified", DateField.dateToString(from)), new Term(
                    "modified", DateField.dateToString(now)), true);

            Searcher searcher = new IndexSearcher("searchIndex");
            Hits hits = searcher.search(query);
            IndexSearchResult result = new IndexSearchResult(hits);
            result.setSearchTypeAndQuery("modified", fromString);
            return result;
        } catch (IOException e) {
            return new ErrorResult("Modified search", "Error doing modified search", e);
        }
    }
}
