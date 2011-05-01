package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.ServletUtilities;

import gw.render.locator.ResourceLocator;
import gw.render.locator.AttachLocator;
import gw.storage.*;

import java.io.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

import org.apache.commons.fileupload.*;

import sun.security.krb5.internal.p;

/**
 * Allows the upload of files from someone's harddisk to the wiki system.
 * 
 * @author Michiel Overeem
 */
public class AttachFile extends GwServlet {
	
	private static final ResourceLocator ATTACH = new AttachLocator();

    /**
     * Handles complete file upload request.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String pathInfo = ServletUtilities.getPathInfo(request);

        try
        {
            if (FileUpload.isMultipartContent(request)) {
                handleMultiPartRequest(request, response, pathInfo);
            }
            
        	response.sendRedirect(request.getContextPath() + "/view" + pathInfo);
        }
        catch(IOException e)
        {
        	print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("Text.Attach"), e.getMessage());        	
        }
    }

    /**
     * Prints the form for uploading file(s). If an parameter "nofields" is
     * present, the form will contain that amount of input fields for files.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {       
    	handleAction(request, response, ATTACH);              
    }

    /**
     * Handles the request if it is a multipartrequest. All files are stored in
     * the given path.
     * 
     * @param request
     *            The multipart request
     * @param response
     *            The response to write exception output to
     * @throws ServletException
     * @throws IOException
     */
    public void handleMultiPartRequest(HttpServletRequest request, HttpServletResponse response,
            String pathInfo) throws ServletException, IOException {

        Storage storage = getSessionContext(request).getSessionStorage();

        DiskFileUpload upload = new DiskFileUpload();

        try {

            List fileItems = upload.parseRequest(request);
            Iterator fileItemIterator = fileItems.iterator();

            while (fileItemIterator.hasNext()) {

                FileItem fileItem = (FileItem) fileItemIterator.next();
                saveUploadedFile(fileItem, pathInfo, storage);               

            }
        } catch (FileUploadException fue) {
            printException(response, fue);
        }        
    }

    /**
     * Handles the saving of one uploaded file.
     * 
     * @param fileItem
     *            The FileItem that has to be saved
     * @param pathInfo
     *            The pathinfo to where the file should be saved
     * @param storage
     *            The storage object to process the saving
     * @return An error string message, or the empty string if no exception
     *         occured
     * @throws IOException
     */
    private void saveUploadedFile(FileItem fileItem, String pathInfo, Storage storage)
            throws IOException {

        long _maxSize = ((Long) getGwContext().getConfigResources().getObject("AttachFile.MaxFileSize")).longValue();

        if (fileItem.getFieldName().startsWith("file") && fileItem.getSize() < _maxSize) {

            try {

                String contenttype = fileItem.getContentType();

                if (!pathInfo.endsWith("/")) {
                    pathInfo += "/";
                }

                String pathToSave = pathInfo + fileItem.getName();
                OutputStream ostream = storage.storeFile(pathToSave);
                ostream.write(fileItem.get());
                ostream.close();
                
                // this "content-type" should stay hard-coded, because every
                // component of the gw
                // uses this property!!
                storage.setProperty(pathToSave, "content-type", contenttype, false);
                
                // We have no control what the user uploads so we tag is as a binary file.
                // This is prevents, that we are going to do a xslt transformation on a file we can't parse.
                storage.setProperty(pathToSave, "isAttachment", "true", false);

            } catch (StorageException se) {
                throw new IOException(fileItem.getName() + getGwContext().getTextResources().getString("Error.CouldNotBeSaved")
                        + ": " + se.toString() + "<br />");
            }
        }       
    }
}