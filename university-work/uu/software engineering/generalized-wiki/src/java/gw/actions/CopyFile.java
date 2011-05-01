package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.ServletUtilities;
import gw.ServletForms;
import gw.storage.*;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * This servlet makes it possible to copy files and directories. This enables
 * the creation of branches.
 * 
 * @author MichielOvereem
 */
public class CopyFile extends GwServlet {

    /**
     * Handles the request of a copy action initiated in the HTML form presented
     * by the doGet method.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        Storage storage = getSessionContext(request).getSessionStorage();

        try {

            // oldpath should be something like: /dir/secondir/ or /dir/file
            // newpath should be something like /newdir
            // newpathfirstpart should be something /existingdir/anotherdir/
            // filename should be something like /filename or /dirname/

            String oldPath = request.getParameter("source");
            String newPath = getNewPath(request);
            String newPathFirstPart = request.getParameter("newPathPart");
            String fileName = request.getParameter("filename");
            String result = "";

            newPath = newPathFirstPart + newPath;

            if ((ServletUtilities.validateInput(oldPath) && ServletUtilities.validateInput(newPath))
                    && (!oldPath.equals(newPath))) {

                result = handlePostRequest(request, response, storage);

            } else {
                result = getGwContext().getTextResources().getString("Error.SameDirectory");
            }

            // if we want to print something, do it:
            if (!result.equals("")) {
                print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("Text.Move"), result);
            } else {
                response.sendRedirect(request.getContextPath() + "/view/" + newPath + fileName);
            }

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);
        } catch (StorageException se) {
            printException(response, se);
        }
    }

    /**
     * Presents the interface to create a copy request.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        handleGetRequest(request, response, "", false);

    }

    /**
     * Handles the Post request. Subclasses should overwrite this method to
     * progress their Post request.
     * 
     * @param request
     *            The request
     * @param response
     *            The response
     * @param storage
     *            The storage object
     * @return An error message or an empty String
     * @throws ServletException
     * @throws IOException
     * @throws StorageException
     */
    protected String handlePostRequest(HttpServletRequest request, HttpServletResponse response,
            Storage storage) throws ServletException, IOException, StorageException {

        String oldPath = request.getParameter("source");
        String newPath = getNewPath(request);
        String newPathFirstPart = request.getParameter("newPathPart");
        String fileName = request.getParameter("filename");
        String result = "";

        newPath = newPathFirstPart + newPath;

        boolean forceCreation = request.getParameter("forceCreation") != null;

        // does the dir exist or do we have to force creation
        // and doesn't there exist a file with the same name already or do
        // we have to overwrite
        if ((forceCreation || (!forceCreation && storage.isDirectory(newPath)))
                && !storage.fileExists(newPath + fileName)) {

            // create the direcotry when it doesn't exist
            if (forceCreation) {
                createDirectory(storage, newPath);
            }

            storage.copyFile(oldPath, newPath + fileName);

        } else {
            if (storage.fileExists(newPath + fileName)) {
                result = getGwContext().getTextResources().getString("Warning.FileExists");
            } else if (!forceCreation && !storage.fileExists(newPath)) {
                result = getGwContext().getTextResources().getString("Warning.DirectoryNotFound");
            }

        }

        return result;

    }

    /**
     * Handles the Get request. Subclasses can overwrite this method. It checks
     * the path, if it exists or if it is the root. When nothing of that is the
     * case, a HTML form is printed.
     * 
     * @param request
     *            The request
     * @param response
     *            The response
     * @param optionalInputs
     *            Optional input fields for the dropdownmenu
     * @throws ServletException
     * @throws IOException
     */
    protected void handleGetRequest(HttpServletRequest request, HttpServletResponse response,
            String optionalInputs, boolean actionSupported) throws ServletException, IOException {

        String pathInfo = ServletUtilities.getPathInfo(request);
        String filename = ServletUtilities.getFileName(pathInfo);
        Storage storage = getSessionContext(request).getSessionStorage();

        try {

            String resultString = "";

            if (pathInfo.equals("/") && !actionSupported) {

                resultString = getGwContext().getTextResources().getString("Error.ActionNotAllowedOnRoot");

            } else if (storage.fileExists(pathInfo)) {

                String pathTemp = ServletUtilities.getRedirectPath(pathInfo);

                if (storage.isDirectory(pathInfo)) {
                    filename = ServletUtilities.getDirName(pathInfo);
                }

                String classAndPack = this.getClass().getName();
                String className = classAndPack.substring(classAndPack.lastIndexOf(".") + 1);

                String dropDownPaths = ServletForms.getDirListingSelect(storage, pathTemp);
                resultString += ServletForms.getReplaceForm(request.getContextPath()
                        + request.getServletPath() + pathInfo, pathInfo, dropDownPaths,
                        optionalInputs, className, getGwContext());

            } else {
                resultString += getGwContext().getTextResources().getString("CopyFileMoveFile.PathNotExists");
            }

            print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("Text.Move"), resultString);

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);
        } catch (StorageException se) {
            printException(response, se);
        }

    }

    /**
     * Gives the path of where the file is copied to
     * 
     * @param request
     *            The request
     * @return The path of where the file is copied to
     */
    protected String getNewPath(HttpServletRequest request) {

        String newPath = request.getParameter("target");

        // let it begin with "/"
        if (!newPath.startsWith("/")) {
            newPath = "/" + newPath;
        }
        // let it end without "/"
        if (!newPath.equals("/") && newPath.endsWith("/")) {
            int idx = newPath.lastIndexOf("/");
            newPath = newPath.substring(0, idx - 1);
        }

        return newPath;

    }

}