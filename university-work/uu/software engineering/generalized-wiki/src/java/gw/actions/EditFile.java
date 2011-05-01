package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.ServletUtilities;
import gw.render.locator.EditLocator;
import gw.render.locator.ResourceLocator;
import gw.storage.*;
import gw.validation.ValidationException;
import gw.validation.XMLValidator;
import gw.validation.XMLValidatorRNG;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Implements the editing of files into the wiki system.
 * 
 * @author Michiel Overeem
 * @author Ivaylo Gochkov
 */
public class EditFile extends GwServlet {
    private static final long serialVersionUID = -4135520001730746522L;
	
	private static final ResourceLocator EDIT = new EditLocator();

    /**
     * Prints the form for editing or creating as file.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        Storage storage = getSessionContext(request).getSessionStorage();
        String pathInfo = ServletUtilities.getPathInfo(request);
        
        try {

            if (!storage.fileExists(pathInfo)) {
                createUnexistingDirectories(storage, pathInfo);
            }
             
            handleAction(request, response, EDIT);
        	
        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);
        } catch (StorageException se) {
            printException(response, se);
        }
    }
     

    /**
     * Generates the output form or message if the file does not exists. This
     * can be a message that the directory is created or an empty HTML form.
     * 
     * @param storage
     *            The Storage object
     * @param pathInfo
     *            The pathInfo
     * @param contextPath
     *            The context path
     * @return The HTML form or message if a file does not exists
     * @throws StorageException
     * @throws IOException
     */
    private void createUnexistingDirectories(Storage storage, String pathInfo)
            throws StorageException, IOException {
        
        // TODO: This method looks obsolete, replace with Storage.ensurePathExists()

        if (pathInfo.endsWith("/")) {
            createDirectory(storage, pathInfo);
            // TODO: How to show that a directory is created
            // _textresources.getString("EditFile.CreatedDirectory") + ": " +
            // pathInfo;
        } else {
            createDirectory(storage, ServletUtilities.getRedirectPath(pathInfo));
        }

    }

}