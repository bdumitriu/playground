package gw.render;

import gw.GwSessionContext;
import gw.storage.Storage;
import gw.storage.StorageException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.jdom.transform.JDOMSource;
import org.jdom.transform.JDOMResult;


/**
 * This Stylesheet is created when no type is registered
 */
public class IdentityStylesheet implements Stylesheet {

    private Storage storage;

    public IdentityStylesheet(GwSessionContext sessioncontext) {
        this.storage = sessioncontext.getSessionStorage();
    }
    
    public String getMimeType(String pathinfo) {
    	String mimetype = "";
    	try {
    		mimetype = (String) storage.getProperties(pathinfo).get("content-type");
    	}
    	catch (StorageException e) {
    		e.printStackTrace();
    	}
    	
    	return mimetype;
    }

    /**
     * Copy the content of a file to an OutputStream
     * 
     * @param pathinfo
     *            The path of the file on which the Stylesheet should be applied
     * @param user
     *            The user to which the file belongs
     * @param output
     *            The output to which the result is written
     * @throws StylesheetApplyException
     */    
    public void apply(Source source, String pathinfo,  Result result)
    		throws StylesheetApplyException {
    	
    	if(source instanceof StreamSource && result instanceof StreamResult)
    	{    		    	    	    
	        try {
	        	InputStream input = ((StreamSource)source).getInputStream();
	        	OutputStream outp = ((StreamResult)result).getOutputStream();
	            byte[] buffer = new byte[64];
	            int dataLength;
	            for (dataLength = input.read(buffer); dataLength > 0; dataLength = input.read(buffer)) {
	            	outp.write(buffer, 0, dataLength);
	            }
	        } catch (IOException e) {
	            throw new StylesheetApplyException(e);
	        }
    	}
    	else if (source instanceof JDOMSource && result instanceof JDOMResult)
    	{
    		JDOMSource jdomsource = (JDOMSource) source;
    		JDOMResult jdomresult = (JDOMResult) result;
    		
    		jdomresult.setResult(jdomsource.getNodes());    		
    	}
    	else
    		throw new StylesheetApplyException("Parameter (source, result) should be instance of (StreamSource, StreamResult) or (JDOMSource, JDOMResult).");
    }
}