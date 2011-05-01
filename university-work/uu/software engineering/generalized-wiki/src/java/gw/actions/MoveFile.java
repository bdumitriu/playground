package gw.actions;

import gw.storage.*;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * This servlet makes it possible to rename and move files and directories.
 * 
 * @author Michiel Overeem
 */
public class MoveFile extends CopyFile {

    /**
     * Handles the progress of the Post request. It performs the move action.
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
        boolean forceOverwrite = request.getParameter("forceOverwrite") != null;

        // does the dir exist or do we have to force creation
        // and doesn't there exist a file with the same name already or do
        // we have to overwrite
        if ((forceCreation || (!forceCreation && storage.isDirectory(newPath)))
                && (forceOverwrite || !storage.fileExists(newPath + fileName))) {

            // create the direcotry when it doesn't exist
            if (forceCreation) {
                createDirectory(storage, newPath);
            }

            if (forceOverwrite && storage.fileExists(newPath + fileName)) {

                InputStream istream = storage.getFile(oldPath);
                byte[] content = new byte[istream.available()];

                istream.read(content);

                OutputStream ostream = storage.storeFile(newPath + fileName);
                ostream.write(content);
                ostream.close();
                storage.deleteFile(oldPath, true);

            } else {

                storage.moveFile(oldPath, newPath + fileName, true);

            }

        } else {
            if (!forceOverwrite && storage.fileExists(newPath + fileName)) {
                System.out.println(newPath + fileName + "  "
                        + storage.fileExists(newPath + fileName));
                result = getGwContext().getTextResources().getString("Warning.FileExists");
            } else if (!forceCreation && !storage.fileExists(newPath)) {
                result = getGwContext().getTextResources().getString("Warning.DirectoryNotFound");
            }

        }

        return result;

    }

    /**
     * Presents the interface to create a move or rename request.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String optionalInput = getGwContext().getTextResources().getString("MoveFile.ForceOverwrite")
                + ": <input type=\"checkbox\" name=\"forceOverwrite\" value=\"force\" /><br />";

        handleGetRequest(request, response, optionalInput, false);

    }

}