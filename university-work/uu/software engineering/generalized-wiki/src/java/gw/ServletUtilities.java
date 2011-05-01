package gw;

import gw.storage.*;
//import gw.storage.javahl.*;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;

import java.util.Map;
import java.util.StringTokenizer;
import java.util.Iterator;
import java.util.SortedSet;

import javax.servlet.http.HttpSession;

import javax.servlet.http.HttpServletRequest;

/**
 * Contains multiple static methods for formatting output and validating. All
 * methods are static and should return something.
 * 
 * @author Michiel Overeem
 * @author Eric Bouwers
 * @author Patrick Camphuijsen
 * @author Jeroen Zuijderwijk
 * @author Ivaylo Gochkov
 * @author The collective Rendering group
 */
public class ServletUtilities {

    /**
     * Checks if the given input String is null, is empty or only consists of
     * spaces.
     * 
     * @param input
     *            The string to test
     * @return True if the string is not null, not empty and not only spaces.
     *         False otherwise
     */
    public static boolean validateInput(String input) {

        boolean valid = false;

        if (input != null) {
            String trimmedInput = input.trim();

            if (trimmedInput.length() != 0) {
                valid = true;
            }
        }

        return valid;
    }

    /**
     * Gives a status text description. Possible texts are "added", "copied",
     * "deleted" and "modified".
     * 
     * @param msg
     *            The StorageStatusMessage
     * @return The status text description from the given StorageStatusMessage
     * @see gw.storage.StorageStatusMessage
     */
    public static String getStatusDescription(StorageStatusMessage msg, GwContext context) {

        if (msg.isAdded()) {
            return context.getTextResources().getString("Status.Added");
        } else if (msg.isCopied()) {
            return context.getTextResources().getString("Status.Copied");
        } else if (msg.isDeleted()) {
            return context.getTextResources().getString("Status.Deleted");
        } else if (msg.isModified()) {
            return context.getTextResources().getString("Status.Modified");
        }

        return context.getTextResources().getString("Status.Normal");

    }

    /**
     * Returns the filename from the given path.<br />
     * Example 1:<br />
     * Input "/somedir/antoherdir/somefile" <br />
     * Output "somefile"<br />
     * Example 2:<br />
     * Input "/somedir/anotherdir/" <br />
     * Output ""<br />
     * 
     * @param path
     *            The given path to substract the filename
     * @return The filename, if it is a file
     */
    public static String getFileName(String path) {

        int idx = path.lastIndexOf("/");

        if (idx >= 0) {
            return path.substring(idx, path.length());
        } else {
            return "";
        }
    }

    /**
     * Returns the directory name of this path. Given a directory path, it
     * returns the last dir name.<br />
     * Example 1:<br />
     * Input "/newDir/newnewDir/newnewnewDir/"<br />
     * Output "/newnewnewDir/"<br />
     * Example 2:<br />
     * Input "/newDir/"<br />
     * Output "/newDir/"<br />
     * 
     * @param path
     *            The given path
     * @return The dir name
     */
    public static String getDirName(String path) {

        String currentDir = getRedirectPath(path);
        return path.substring(currentDir.length());

    }

    /**
     * Returns a path, one directory above the given path.<br />
     * Example 1:<br />
     * Input "/newDir/newnewDir/newnewnewDir/"<br />
     * Output "/newDir/newnewDir/"<br />
     * Example 2:<br />
     * Input "/newDir/<br />
     * Output "/"<br />
     * 
     * @param path
     *            The given path
     * @return The path to the directory above the given path
     */
    public static String getRedirectPath(String path) {
        String result = "";
        if (path.equals("/")) {
            result = "/";
        } else {
            if (path.endsWith("/")) {
                // if(path.length() > 2) {
                result = path.substring(0, path.substring(0, path.length() - 2).lastIndexOf('/'));
                /*
                 * } else { result = "/"; }
                 */
            } else {
                result = path.substring(0, path.lastIndexOf('/'));
            }
        }
        return result;

    }

    /**
     * Splits a path in to an array.<br />
     * Example 1: <br />
     * Input "/onedir/twodir/threedir/"<br />
     * Output {"onedir", "twodir", "threedir"}<br />
     * 
     * @param path
     *            The given path
     * @return The array containing the splitted path
     */
    public static String[] splitPathInArray(String path) {

        StringTokenizer dirs = new StringTokenizer(path, "/");

        int length = dirs.countTokens();
        String[] resultDirs = new String[length];
        int counter = 0;

        while (dirs.hasMoreTokens()) {

            resultDirs[counter] = dirs.nextToken();
            counter++;

        }

        return resultDirs;
    }

    /**
     * Does the opposite of splitPathInArray. Example: {"onedir", "twodir",
     * "threedir", "fourdir"}, 3 becomes: "onedir/twodir/threedir/"
     */
    public static String joinPathFromArray(String[] path_part, int end) {
        String result = "";
        for (int i = 0; i < end; i++)
            result += path_part[i] + "/";
        return result;
    }

    /**
     * Returns a list of all the revision numbers this file has.
     * 
     * @param storage
     *            the storage object to retrieve the logs from
     * @param path
     *            the path from which the revision numbers are requested
     * 
     * @return the array with revision numbers.
     */
    public static long[] getRevisionNumbers(Storage storage, String path) throws StorageException {

        SortedSet logs = storage.getLog(path);
        long[] revisions = new long[logs.size()];

        Iterator iterator = logs.iterator();
        int count = 0;

        while (iterator.hasNext()) {
            StorageLogMessage sslm = (StorageLogMessage) iterator.next();
            revisions[count] = sslm.getRevisionNumber();
            count++;
        }

        return revisions;
    }

    /**
     * Calls normalizedPathInfo with "/" as the baseDir
     * 
     * @see ServletUtilities#normalizePathInfo(Storage, String, String)
     */
    public static String normalizePathInfo(Storage storage, String pathinfo)
            throws StorageException {
        return normalizePathInfo(storage, pathinfo, "/");
    }

    /**
     * Normalized the pathinfo string and ensure that the return value fits the
     * following requirements: - absolute path - directory path ends by a slash,
     * file doesn't
     * 
     * <b>Note:<b> if pathinfo points to a non existing directory it will not
     * end with a slash
     * 
     * @param pathinfo
     * @param storage
     * @param baseDir
     * @return a normalized pathinfo if the pathinfo is relative baseDir is
     *         prepended
     * @throws StorageException
     */
    public static String normalizePathInfo(Storage storage, String pathinfo, String baseDir)
            throws StorageException {
        if (!pathinfo.startsWith("/"))
            pathinfo = "/" + baseDir + "/" + pathinfo;

        // we evaluate the current and previous dir references by
        // setting directory names which should not be included to null
        String[] pathParts = splitPathInArray(pathinfo);

        for (int i = 0; i < pathParts.length; i++) {
            if (pathParts[i].equals(".")) { // evaluate current dir references
                pathParts[i] = null;
            } else if (pathParts[i].equals("..")) { // evaluate previous dir
                // references
                pathParts[i] = null;
                int prevDirIndex = getPrevNotNullIndex(pathParts, i);
                if (prevDirIndex >= 0)
                    pathParts[prevDirIndex] = null;
            }
        }

        String result = "";
        for (int i = 0; i < pathParts.length; i++)
            if (pathParts[i] != null)
                result += "/" + pathParts[i];

        // defaults to root
        if (result.equals(""))
            result = "/";

        // directories end with a slash
        if (storage.isDirectory(result) && !result.equals("/"))
            result = result + "/";

        return result;
    }

    /**
     * Returns the pathinfo of the given request. Maps the root to "/".
     * 
     * @param request
     *            The HttpServletRequest from which the pathinfo needs to be
     *            extracted.
     * @return A string with the pathinfo of the given request.
     */
    public static String getPathInfo(HttpServletRequest request) {
        String pathInfo = request.getPathInfo();
        if (pathInfo == null) {
            pathInfo = "/";
        }
        return pathInfo;
    }

    /**
     * Look before start index to find an object which is not null. This
     * function is created for normalization
     * 
     * @return the index of a not null object in the array or -1 if none exists.
     * @see ServletUtilities#normalizePathInfo(Storage, String, String)
     */
    private static int getPrevNotNullIndex(Object[] objects, int startIndex) {
        for (startIndex--; startIndex >= 0 && objects[startIndex] == null; startIndex--)
            ;
        return startIndex;
    }

    /**
     * Returns the action path used for some forms
     * 
     * @param request
     *            The HttpServletRequest from which we get the action path
     * @return The action path
     */
    public static String getActionPath(HttpServletRequest request) {
        String contextPath = request.getContextPath();
        String servletPath = request.getServletPath();
        String path = ServletUtilities.getPathInfo(request);
        return contextPath + servletPath + path;
    }

    /**
     * Sets the referer of a request
     * 
     * @param request
     *            The HttpServletRequest for which we set the referer
     */
    public static void setReferer(HttpServletRequest request) {
        String referer = request.getServletPath() + getPathInfo(request);

        HttpSession session = request.getSession();
        session.setAttribute("referer", referer);
    }

    /**
     * Returns the content type of the path, or the GWML content type if the
     * file does not exist.
     */
    public static String getContentType(Storage storage, String path) {
        String contentType = null;
        try {
            Map<String, String> map = storage.getProperties(path);
            if (map != null)
                contentType = map.get("content-type");

            if (contentType == null)
                contentType = GwConstants.GWML_MIME_TYPE;
        } catch (StorageException se) {
            contentType = GwConstants.GWML_MIME_TYPE;
        }
        return contentType;
    }

    public static String escapeSpecials(String input) {
        StringBuffer filtered = new StringBuffer(input.length());
        char c;

        for (int i = 0; i < input.length(); i++) {
            c = input.charAt(i);
            switch (c) {
            case '<':
                filtered.append("&lt;");
                break;
            case '>':
                filtered.append("&gt;");
                break;
            case '"':
                filtered.append("&quot;");
                break;
            case '&':
                filtered.append("&amp;");
                break;
            default:
                filtered.append(c);
            }
        }
        return filtered.toString();
    }
    
    /**
     * Reads the content available in a InputStream
     * We don't close it so you should do that on your own
     * 
     * @param input the stream to read from
     * @return the content of the stream as represented by a String
     *         null if the InputStream given is null
     * @throws IOException if the input stream provides binary shit in stead of ASCII
     */
    public static String read(InputStream input) throws IOException
    {
		if(input != null) {
					
		   StringBuffer buf = new StringBuffer(1024);
		   BufferedReader dataReader = new BufferedReader(new InputStreamReader(input));
		   int temp;
		
		   // get the whole file in the string
		   while((temp = dataReader.read()) != -1)
		   {
		      if(temp > 255 || temp < -1)
		         throw new IOException("Binary content found, ASCII Expected.");
		      buf.append((char)temp);				       
		   }
		
		   return buf.toString();	
		}
		else
			return null;
	}
}
