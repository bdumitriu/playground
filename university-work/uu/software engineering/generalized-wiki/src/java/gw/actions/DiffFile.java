package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.ServletForms;
import gw.ServletUtilities;
import gw.storage.*;

import java.lang.String;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Implements the functionality of viewing the differences of a file of two
 * revisions.
 * 
 * @author Eric Bouwers
 * @author Jeroen Zuijderwijk
 */
public class DiffFile extends GwServlet {

    /**
     * Prints the differences of a file between two revisions
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String contextPath = request.getContextPath();
        String servletPath = request.getServletPath();
        String pathInfo = ServletUtilities.getPathInfo(request);
        Storage storage = getSessionContext(request).getSessionStorage();

        String result = "";

        try {

            String dropdown1 = ServletForms.getRevisionNumbersSelect("rev1", ServletUtilities
                    .getRevisionNumbers(storage, pathInfo));
            String dropdown2 = ServletForms.getRevisionNumbersSelect("rev2", ServletUtilities
                    .getRevisionNumbers(storage, pathInfo));

            try {

                long rev1 = Long.parseLong(request.getParameter("rev1"));
                long rev2 = Long.parseLong(request.getParameter("rev2"));

                if (rev1 == rev2) {

                    result = ServletForms.getDiffForm(ServletUtilities.getActionPath(request),
                            dropdown1, dropdown2, getGwContext().getTextResources()
                                    .getString("Error.IntervalTooSmall"), getGwContext());

                } else {

                    result = "<pre>"
                            + getDiff(storage, contextPath, servletPath, pathInfo, Math.min(rev1,
                                    rev2), Math.max(rev1, rev2)) + "</pre>";

                }
            } catch (NumberFormatException nfe) {

                result = ServletForms.getDiffForm(ServletUtilities.getActionPath(request),
                        dropdown1, dropdown2, getGwContext().getTextResources().getString("Error.NoNumber"), getGwContext());

            }

        } catch (StorageException se) {

            printException(response, se);
        }

        print(response, GwConstants.XHTML_MIME_TYPE, "title", result);

    }

    /**
     * Prints a form for specifying the revisions of which you wish to view the
     * revisions.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        Storage storage = getSessionContext(request).getSessionStorage();
        String pathInfo = ServletUtilities.getPathInfo(request);

        try {

            String result = "";

            if (storage.fileExists(pathInfo)) {

                String dropdown1 = ServletForms.getRevisionNumbersSelect("rev1", ServletUtilities
                        .getRevisionNumbers(storage, pathInfo));
                String dropdown2 = ServletForms.getRevisionNumbersSelect("rev2", ServletUtilities
                        .getRevisionNumbers(storage, pathInfo));
                result = ServletForms.getDiffForm(ServletUtilities.getActionPath(request),
                        dropdown1, dropdown2, "", getGwContext());

            } else {
                result = getGwContext().getTextResources().getString("Error.FileNotFound");
            }

            print(response, GwConstants.XHTML_MIME_TYPE, "title", result);

        } catch (StorageException se) {
            printException(response, se);
        }

    }

    /**
     * Prints the differences between two revisions.
     * 
     * @param storage
     *            The SVNStorage object
     * @param response
     *            The handler for our responses to the client
     * @param contextPath
     *            The root of the filesystem
     * @param servletPath
     *            The path inside the servlet
     * @param path
     *            The path of the file
     * @param rev1
     *            Revision number of the basic revision
     * @param rev2
     *            Revision number of the revision we want to compare to
     * @throws IOException
     */
    private String getDiff(Storage storage, String contextPath, String servletPath, String path,
            long rev1, long rev2) throws IOException {

        String result = "";

        try {
            String fileDiff = storage.getFileDiff(path, rev1, rev2);

            if (path == null) {
                path = "/";
            }

            String filename = ServletUtilities.getFileName(path);

            // look if the file exists
            if (storage.fileExists(path)) {

                result += getGwContext().getTextResources().getString("DiffFile.ChangeBetweenRevisions") + " " + rev1
                        + " " + getGwContext().getTextResources().getString("DiffFile.AndRevision") + " " + rev2 + " "
                        + getGwContext().getTextResources().getString("DiffFile.OfFile") + " " + path + "<br />"
                        + fileDiff.replaceAll("\n", "<br />");

            } else {

                result = getGwContext().getTextResources().getString("Error.FileNotFound");

            }

        } catch (StorageException se) {
            result = ServletForms.getDiffForm(contextPath, servletPath, path, getGwContext().getTextResources()
                    .getString("Error.RevisionNotFound"), getGwContext());
        }

        return result;
    }

}