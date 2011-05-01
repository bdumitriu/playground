package gw.actions;

import gw.GwServlet;


/**
 * Implements viewing history information for a file or directory. This should
 * get a parameter "revision" with possible parameters for the path. It then
 * shows the state of that file or dir at that revision.
 * 
 * FIXME: this class must be revived.
 * 
 * @author Michiel Overeem
 */
public class HistoryFile extends GwServlet {

    /**
     * The doGet handles the requests made to this servlet. It interacts with
     * the Render component to create a response.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        Storage storage = getStorage(request);
        String pathInfo = ServletUtilities.getPathInfo(request);

        String filename = ServletUtilities.getFileName(pathInfo);
        long revision = 0;

        try {
            revision = Long.parseLong(request.getParameter("revision"));
        } catch (NumberFormatException nfe) {
        }

        try {

            String result = "";

            if (storage.fileExists(pathInfo)) {

                result = handleFileExists(storage, revision, pathInfo, request);

            } else {
                result = getGwContext().getTextResources().getString("Error.FileNotFound");
            }

            print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("Text.History"), result);

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);
        } catch (StorageException se) {
            printException(response, se);
        } catch (TransformerException te) {
            printException(response, te);
        }

    }
     */
    
    /**
     * Handles the output if the file exists.
     * 
     * @return The history information of the file, or the form if it could not
     *         be retrieved
    private String handleFileExists(Storage storage, long revision, String pathInfo,
            HttpServletRequest request) throws StorageException, IOException, TransformerException {
        String result = "";

        if (revision != 0) {

            if (storage.isDirectory(pathInfo)) {
                result = handleDir(request.getContextPath(), storage, pathInfo, revision);
            } else {
                result = handleFile(storage, pathInfo, revision);
            }

            result = createXML(storage, pathInfo, result, request.getRequestURI(),
                    "/history-template.xsl");

        } else {

            String dropdown = ServletForms.getRevisionNumbersSelect("revision", ServletUtilities
                    .getRevisionNumbers(storage, pathInfo));
            result = ServletForms.getHistoryForm(ServletUtilities.getActionPath(request), dropdown, getGwContext());

        }

        return result;
    }
     */    

    /**
     * Handles the process of a directory.
     * 
     * @param contextPath
     *            Information about the path in the container
     * @param storage
     *            The SVNStorage object
     * @param pathInfo
     *            The path information
     * @param revision
     *            The revision number
     * @return The rendered information
     * @throws StorageException
    private String handleDir(String contextPath, Storage storage, String pathInfo, long revision)
            throws StorageException {

        Map dirs = storage.getDirListing(pathInfo, revision, false);
        Iterator paths = dirs.keySet().iterator();
        String dirResult = "";
        String filesResult = "";

        while (paths.hasNext()) {

            String path = (String) paths.next();

            if (((StorageDirEntry) dirs.get(path)).isDirectory()) {

                dirResult += "<a href=\"" + contextPath + "/view" + path + "/\">" + path
                        + "/</a><br />";

            } else {

                filesResult += "<a href=\"" + contextPath + "/view" + path + "\">" + path
                        + "</a><br />";

            }

        }
        return dirResult + "<br />" + filesResult;

    }
     */    
}
