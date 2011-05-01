package gw.actions;

import gw.GwServlet;
import gw.ServletUtilities;
import gw.storage.*;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

import java.util.Enumeration;

/**
 * Provides a way to set conflicted files resolved.
 * 
 * @author Eric Bouwers
 */
public class ResolvedFile extends GwServlet {

    /**
     * Sets the files resolved that the user has selected. The request should
     * contain filenames that are selected in the CommitServlet by the user.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String path = ServletUtilities.getPathInfo(request);
        Enumeration solved = request.getParameterNames();

        Storage storage = getSessionContext(request).getSessionStorage();
        // Storage storage =
        // SessionStorageFactory.getUnsecuredStorageFromSession(
        // request.getSession() );

        try {

            while (solved.hasMoreElements()) {

                String file = (String) solved.nextElement();

                if (storage.fileExists(file)) {
                    storage.setResolved(file);
                } else {
                }

            }

            response.sendRedirect(request.getContextPath() + "/commit/");

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);
        } catch (StorageException se) {
            printException(response, se);
        }

    }

}