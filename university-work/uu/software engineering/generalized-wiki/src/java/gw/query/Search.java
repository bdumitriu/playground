package gw.query;

import gw.GwConstants;
import java.io.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Search allows one to search in the GW. This class basically is "just" a
 * dispatcher to call the real implementations for the search. We allow two
 * types of search: simple index searching and more advanced searchers. The
 * simple search of the index is easy to implement: make sure a field exists
 * with the information you want to search for. Then you can search this with
 * the IndexSearch. More advanced search is done by implementing the
 * SearchHandler. These search types should then be added to the searchHandlers
 * map (in the code of search). Maybe in the future we will support dynamic
 * loading. The index is connected to the rest of the wiki via events that get
 * sent when someone updates a file.
 * 
 * @see gw.query.SearchHandler
 * @see gw.query.IndexSearch
 * @see gw.query.IndexUpdater
 */
public class Search extends HttpServlet
{
    private Map searchHandlers;
    private Set luceneSearch;

    /**
     * The Lucene index is stored in a directory with this name. This path is
     * passed verbatim to Lucene, which seems to pass it verbatim to the Java IO
     * system, so this path is relative to where the server is run from.
     */
    public static final String indexPath = "searchIndex";

    /**
     * Initializes the search class. What needs to be done here is adding all
     * different kind of searches to the appriopriate maps and sets.
     */
    public void init()
    {
        searchHandlers= new HashMap();
        searchHandlers.put("indexstats", new IndexStatsQuery());
		// searchHandlers.put("link",new
        // LinkSearch());http://www.cs.uu.nl/wiki/Gw/RenderingReport
		searchHandlers.put("modified", new ModifiedSearch());
		searchHandlers.put("livepages", new LivePagesQuery());
        searchHandlers.put("deadpages", new DeadPagesQuery());


        luceneSearch = new HashSet();
        luceneSearch.add("topic");
        luceneSearch.add("content");
    }

    /**
     * We want to support both searching by a post and by a get. This request is
     * just forwarded to doRequest.
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    { 
        doRequest(request, response);
    }

     /**
         * We want to support both searching by a post and by a get. This
         * request is just forwarded to doRequest.
         */
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
        doRequest(request, response);
    }

    /**
     * Handles the request for a search. The type of a response currently is
     * text/html This might be changed to output using GWML or WikiCode.
     * 
     * @param request
     *            The HttpServletRequest that initiated this servlet
     * @param response
     *            The Response to which the output should be send.
     */
    public void doRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
        response.setContentType(GwConstants.XHTML_MIME_TYPE);
        PrintWriter out = response.getWriter();

        doSearch( request.getParameterMap(), out, SearchUtil.isQuiet(request.getParameterMap()));
    }


    /*
     * Returns a stringbuffer with the results for the search. The searc has to
     * be specified in the parameterMap. @param parameterMap Parameter Map
     * containing the parameters for the search. @return with all output from
     * the search.
     */
    public StringBuffer search(Map parameterMap) {
        StringWriter sw = new StringWriter();
        PrintWriter printer = new PrintWriter(sw);
        doSearch(parameterMap, printer, true);

        return sw.getBuffer();
    }

    /*
     * Initiates a search, and prints the result to the specified printWriter.
     * @param parameterMap Containg the parameters for the search @param
     * printWriter The printWriter to which you want to write the output. @param
     * embedded When true, we will treat this request like it is an embedded
     * search request (thus not showing the query).
     */
    public void doSearch(Map parameterMap, PrintWriter printWriter, boolean embedded) {
        SearchResult result;
        try {
            result = handleSearch( parameterMap);
        } catch(IOException ioe) {
            result = new ErrorResult("Search", "Error while trying to search, make sure the index exists.", ioe);
        }
        if(! embedded) {
            result.showQuery(printWriter);
        }
        result.showResult(printWriter);
    }

    /*
     * Handle the search request
     */
    private SearchResult handleSearch(Map parameterMap) throws IOException
    {
        String searchType = SearchUtil.getSearchType(parameterMap);
        String queryString = SearchUtil.getQueryString(parameterMap);
        if(searchHandlers.containsKey(searchType))
        {
            SearchHandler handler = (SearchHandler)searchHandlers.get(searchType);
            return handler.search(parameterMap);
        }
        else if(queryString.equals(""))
        {
            return showIndexStats();
        }else
        {
            if(!luceneSearch.contains(searchType))
            {
                return new ErrorResult("No such type of search: "+searchType);
            }
            IndexSearch searcher = new IndexSearch();
            return searcher.searchIndex(searchType, queryString);
        }
    }


    /**
     * Shows the statistics for the index. Since we have some searchhandler for
     * this already, we can just use that one.
     * 
     * @return The SearchResult containing the statistics for the index.
     */
    public SearchResult showIndexStats() throws IOException
    {
         SearchHandler indexstats = (SearchHandler)searchHandlers.get("indexstats");
         return indexstats.search(null);
    }
}
