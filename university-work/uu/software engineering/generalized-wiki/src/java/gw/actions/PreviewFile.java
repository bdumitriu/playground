package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.GwSessionContext;
import gw.ServletUtilities;

import java.io.*;
import java.util.HashMap;

import javax.servlet.*;
import javax.servlet.http.*;

import gw.render.ProxyStorage;
import gw.render.locator.EditLocator;
import gw.render.locator.PreviewLocator;
import gw.render.locator.ResourceLocator;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.validation.ValidationException;
import gw.validation.XMLValidator;
import gw.validation.XMLValidatorRNG;

/**
 * Provides a way of previewing changes before saving.
 * 
 * @author Eelco Visser
 */
public class PreviewFile extends GwServlet {
	
	private static final ResourceLocator PREVIEW = new PreviewLocator();

    /**
     * Shows the posted text in a preview template. Text is rendered. By hitting
     * the back button in the browser, users can edit the text again.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
    	
        Storage storage = getSessionContext(request).getSessionStorage();

        ProxyStorage proxyStorage = new ProxyStorage(storage);
        GwSessionContext sessionContext = getSessionContext(request); 
        sessionContext.setSessionStorage(proxyStorage);
        String pathInfo = ServletUtilities.getPathInfo(request);
        String validate = request.getParameter("validate"); 
        
        try {
            createVirtualFile(request, proxyStorage);        	

            if (validate != null && validate.equals("true"))
            {
                 validate(proxyStorage, pathInfo, response);
            }
            
            handleAction(request, response, PREVIEW);	 
        } catch (StorageException se) {
            printException(response, se);
        } catch (IOException e) {
            printException(response, e);
        } catch (ValidationException e) {
            printException(response, e);
        }
        finally 
        {
        	sessionContext.setSessionStorage(storage);        	
        }
        
    }
    
    private void validate(Storage storage, String pathinfo, HttpServletResponse response) throws IOException, ValidationException, StorageException
    {
        String contentType = ServletUtilities.getContentType(storage, pathinfo);
        boolean valid = false;
        //create factory, returning a schema file given a mimetype. Select a validator based on the schema file
        if (contentType.equals(GwConstants.GWML_MIME_TYPE))
        {
            InputStream rngSchema = getClass().getClassLoader().getResourceAsStream("/gw/schemas/gwml.rng"); 
            InputStream xmlSource = storage.getFile(pathinfo);                    
            XMLValidator rngValidator = new XMLValidatorRNG(xmlSource, rngSchema);
            valid = rngValidator.validate();
            
            if (!valid)
            {
            	String validationResults = rngValidator.getValidationResults();
                throw new ValidationException("<pre>"+validationResults+"</pre>");
            }
        }
    }
        
    /**
     * @param request
     * @param proxyStorage
     * @throws StorageException
     */
	private void createVirtualFile(HttpServletRequest request, ProxyStorage proxyStorage)
            throws StorageException, IOException {

        String pathInfo = ServletUtilities.getPathInfo(request);
        String text = request.getParameter("text");

        ByteArrayInputStream textStream = new ByteArrayInputStream(text.getBytes());

        String contentType = request.getParameter("contenttype");

        HashMap<String, String> propertyMap = new HashMap<String, String>();
        propertyMap.put("content-type", contentType);
        proxyStorage.addVirtualFile(pathInfo, textStream, propertyMap);
    }
}
