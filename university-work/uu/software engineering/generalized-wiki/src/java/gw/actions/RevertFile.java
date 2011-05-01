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
 * Enables reverting of a certain file or directory to a revision in the past.
 * 
 * @author Jeroen Zuijderwijk
 */
public class RevertFile extends GwServlet {

    /**
     * Handles the revert request. It shows the form again if the revision
     * numbers aren't correct.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String contextPath = request.getContextPath();
        String servletPath = request.getServletPath();
        String pathInfo = ServletUtilities.getPathInfo(request);

        long rev = (long) Integer.parseInt(request.getParameter("revision"));

        Storage storage = getSessionContext(request).getSessionStorage();

        try {
            storage.revertFile(pathInfo, rev);

            response.sendRedirect(contextPath + "/view" + pathInfo);
        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);

        } catch (StorageException se) {
            response.sendRedirect(contextPath + "/revert" + pathInfo + "?error");
        }

    }

    /**
     * Provides a interface for the user to select a revision to revert to.
     * 
     * @param request
     *            A http post request
     * @param response
     *            The handler for the responses to the client
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String contextPath = request.getContextPath();
        String servletPath = request.getServletPath();
        String pathInfo = ServletUtilities.getPathInfo(request);
        Storage storage = getSessionContext(request).getSessionStorage();

        String result = "";

        try {

            String errorMessage = "";
            if (request.getParameter("error") != null) {
                errorMessage = getGwContext().getTextResources().getString("Error.RevisionNotFound");
            }

            if (storage.fileExists(pathInfo)) {
                String dropdown = ServletForms.getRevisionNumbersSelect("revision",
                        ServletUtilities.getRevisionNumbers(storage, pathInfo));

                result = ServletForms.getRevertForm(dropdown, contextPath, servletPath, pathInfo,
                        errorMessage, getGwContext());
            } else {
                result = getGwContext().getTextResources().getString("Error.FileNotFound");
            }

            print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("Text.Revert"), result);

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);

        } catch (StorageException se) {
            printException(response, se);
        }

    }

}