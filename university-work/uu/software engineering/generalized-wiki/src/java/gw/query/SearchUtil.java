package gw.query;

import java.util.*;

/** Provides some utility functions used in searching.
*/
public class SearchUtil{

    /** Gets the searchtype, set in the parameter type.
    * If no such type is set, return an empty string.
    */
    public static String getSearchType(Map parameterMap)
    {
         return getParameter(parameterMap, "type");
    }

    /** Gets the query, set in the parameter query.
   * If no query is given, return an empty string.
   */
    public static String getQueryString(Map parameterMap)
    {
        return getParameter(parameterMap, "query");
    }

    /** Find out whether the user wants to do a quiet search.
    */
    public static boolean isQuiet(Map parameterMap)
    {
        String quiet = getParameter(parameterMap, "quiet");
        if(quiet.equals("no") || quiet.equals(""))
        {
            return false;
        }

        return true;
    }

    /**Returns the first String with parameter name parameterName.
    * @param parameterMap The map with parameters
    * @param parameterName The parameter to search for.
    */
    public static String getParameter(Map parameterMap, String parameterName) {
        if(parameterMap.get(parameterName) == null )
            return "";

        String values[] = (String[])parameterMap.get(parameterName);
        return values[0];
    }

}
