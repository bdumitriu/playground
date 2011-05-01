package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.ServletForms;
import gw.ServletUtilities;
import gw.storage.*;

import java.util.Iterator;
import java.lang.String;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.text.SimpleDateFormat;

/**
 * Prints the praise log for a file.
 * 
 * @author Eric Bouwers
 * @author Jeroen Zuijderwijk
 * @author Michiel Overeem
 */
public class PraiseFile extends GwServlet {

    /**
     * Prints a form for specifying the revision numbers needed for the praise
     * log.
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

            if (request.getParameter("from") != null && request.getParameter("to") != null) {

                long to = 0;
                long from = 0;

                try {

                    from = Long.parseLong(request.getParameter("from"));
                    to = Long.parseLong(request.getParameter("to"));

                } catch (NumberFormatException nfe) {
                    result = ServletForms.getPraiseForm(storage, contextPath, servletPath,
                            pathInfo, getGwContext().getTextResources().getString("Error.NoNumber"), getGwContext());
                }

                if (to == from) {

                    result = ServletForms.getPraiseForm(storage, contextPath, servletPath,
                            pathInfo, getGwContext().getTextResources().getString("Error.IntervalTooSmall"), getGwContext());

                } else {

                    try {

                        Iterator praise = storage.blame(pathInfo, Math.min(from, to), Math.max(
                                from, to));
                        result = printPraiseInfo(praise, pathInfo);

                    } catch (StorageException se) {

                        result = ServletForms.getPraiseForm(storage, contextPath, servletPath,
                                pathInfo, getGwContext().getTextResources().getString("Error.RevisionNotFound"), getGwContext());
                    }
                }

            } else {
                result = ServletForms
                        .getPraiseForm(storage, contextPath, servletPath, pathInfo, "", getGwContext());
            }

            print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("Text.Praise"), result);

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);

        } catch (StorageException se) {
            printException(response, se);
        }

    }

    /**
     * Prints the praise log for a specified file.
     * 
     * @param storage
     *            The storage object of the handled working copy
     * @param praise
     *            The praise iterator from which the information can be
     *            substracted
     * @param path
     *            The path of the file
     * @param response
     *            The handler for the responses to the client
     * @return The praise log
     * @throws IOException
     */
    private String printPraiseInfo(Iterator praise, String path) {

        String result = "";
        StorageBlameLine praiseLine = null;
        int line = 1;
        result += "<table><tr><td>" + getGwContext().getTextResources().getString("Text.Line") + ":</td><td>"
                + getGwContext().getTextResources().getString("Text.Author") + ":</td><td>"
                + getGwContext().getTextResources().getString("Text.Revision") + ":</td><td>"
                + getGwContext().getTextResources().getString("Text.Changed") + ":</td><td>"
                + getGwContext().getTextResources().getString("Text.Content") + ":</td></tr>";

        while (praise.hasNext()) {

            praiseLine = (StorageBlameLine) praise.next();
            result += "<tr><td>"
                    + line
                    + "</td><td>"
                    + praiseLine.getAuthor()
                    + "</td><td>"
                    + praiseLine.getRevision()
                    + "</td><td>"
                    + ((SimpleDateFormat) getGwContext().getConfigResources().getObject("Date.Format"))
                            .format(praiseLine.getChanged()) + "</td><td>" + praiseLine.getLine()
                    + "</td></tr>";
            line++;

        }

        result += "</table>";

        return result;

    }

}