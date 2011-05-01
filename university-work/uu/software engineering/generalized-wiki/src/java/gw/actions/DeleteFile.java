package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.ServletForms;
import gw.ServletUtilities;
import gw.storage.*;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Implements the delete functionality for the wiki system.
 * 
 * @author Michiel Overeem
 * @author Eric Bouwers
 * @author Jeroen Zuijderwijk
 * @author Patrick Camphuijsen
 */
public class DeleteFile extends GwServlet {

    /**
     * Delete a file or directory.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        PrintWriter out = response.getWriter();
        Storage storage = getSessionContext(request).getSessionStorage();
        String pathInfo = ServletUtilities.getPathInfo(request);

        String output = "";
        String submit = request.getParameter("submit");

        if (submit != null && submit.equals(getGwContext().getTextResources().getString("YesKey"))) {

            try {

                storage.deleteFile(pathInfo, true);

            } catch (InsufficientStorageAccessPermissionsException isape) {
                ServletUtilities.setReferer(request);
                getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(
                        request, response);
            } catch (StorageException se) {
                printException(response, se);
            }

            response.sendRedirect(request.getContextPath() + "/view/"
                    + ServletUtilities.getRedirectPath(pathInfo));

        } else if (submit != null && submit.equals(getGwContext().getTextResources().getString("NoKey"))) {

            response.sendRedirect(request.getContextPath() + "/view/" + pathInfo);

        }
    }

    /**
     * Prints the form for deletion of a file or directory.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        PrintWriter out = response.getWriter();
        Storage storage = getSessionContext(request).getSessionStorage();
        String pathInfo = request.getPathInfo();

        String output = "";

        if (pathInfo == null) {
            pathInfo = "/";
        }

        try {

            if (pathInfo.equals("/")) {

                output = getGwContext().getTextResources().getString("Error.DontDeleteRoot");

            } else if (storage.fileExists(pathInfo)) {

                String actionPath = request.getContextPath() + request.getServletPath() + pathInfo;
                output = ServletForms.getDeleteForm(storage.isDirectory(pathInfo), actionPath, getGwContext());

            } else {
                output = getGwContext().getTextResources().getString("Error.FileNotFound");
            }

            print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("Text.Delete") + " " + pathInfo,
                    output);

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);
        } catch (StorageException se) {
            printException(response, se);
        }
    }

}