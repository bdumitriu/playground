package gw.render.locator;

import gw.GwContext;
import gw.GwSessionContext;
import gw.ServletUtilities;
import gw.render.parsers.ParseException;
import gw.render.Stylesheet;
import gw.render.StylesheetCreateException;
import gw.render.StylesheetFactory;
import gw.render.TransformationResource;
import gw.render.parsers.Parser;
import gw.storage.Storage;
import gw.storage.StorageException;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.jdom.transform.JDOMSource;

public class StatusLocator extends ActionResourceLocator{
	

	public TransformationResource locate(GwContext context, GwSessionContext sessionContext, String pathinfo) throws StylesheetCreateException, StorageException, ParseException
	{
		Storage storage = sessionContext.getSessionStorage();				
    	String contentType = ServletUtilities.getContentType(storage, pathinfo);    	  
    	
    	Stylesheet stylesheet;  
    	Source source;

		if (storage.fileExists(pathinfo))
		{						   
    		source = new JDOMSource(formatStatuses(storage.getStatus(pathinfo, true), false, storage, pathinfo, context));	    			
    		stylesheet = getStylesheet(context, sessionContext, pathinfo, "status", "_" + contentType);	    			    			    			    			    			           	          		        	      
		}
        else
        {
        	source = buildError("StatusOfNonExistingFile");
			stylesheet = getStylesheet(context, sessionContext, pathinfo, "error", "");
        }	
		
		return new TransformationResource(stylesheet, source);
	}
}
