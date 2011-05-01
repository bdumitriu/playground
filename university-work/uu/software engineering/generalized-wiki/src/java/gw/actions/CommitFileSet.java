package gw.actions;

import gw.GwServlet;
import gw.ServletUtilities;
import gw.render.locator.ResourceLocator;
import gw.render.locator.CommitLocator;
import gw.storage.*;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Makes it possible to commit a set of changes.
 * 
 * @author Michiel Overeem
 * @author Patrick Camphuijsen
 */
public class CommitFileSet extends GwServlet {
	
	private static final ResourceLocator COMMIT = new CommitLocator();
	

    /**
     * Initialize the servlet. It calls the init in HandleFile.
     * 
     * @throws ServletException
     */
    public void init() throws ServletException {
        super.init();
        StorageListenerContainer.addListener(new gw.query.EventHandler());
    }

    /**
     * Commits the current local repository.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        Storage storage = getSessionContext(request).getSessionStorage();
        String pathInfo = ServletUtilities.getPathInfo(request);

        String logmsg = request.getParameter("logmsg");

        if (logmsg != null && logmsg.length() > 0) {

            try {

                storage.commit(logmsg);               
                response.sendRedirect(request.getContextPath() + "/view" + pathInfo);
            } catch (StorageException se) {

                printException(response, se);
            }
        } else {

            response.sendRedirect(request.getContextPath() + "/commit" + pathInfo + "?noMessage");

        }
    }

    /**
     * Shows a list of changes and conflicts with a box for a commit message.
     * 
     * @param request
     *            The servlet request for the page
     * @param response
     *            The object through which the response can be returned to the
     *            servlet
     * @throws ServletException
     * @throws IOException
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
    	
    		handleAction(request, response, COMMIT);
    	 
    }
}