package gw.render.locator;

import gw.GwContext;
import gw.GwSessionContext;
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

public class AttachLocator extends ActionResourceLocator{
	

	public TransformationResource locate(GwContext context, GwSessionContext sessionContext, String pathinfo) throws StylesheetCreateException, StorageException, ParseException
	{
		Storage storage = sessionContext.getSessionStorage();
    	
    	Stylesheet stylesheet;  
    	Source source;

		if (storage.fileExists(pathinfo))
		{						   
			if(storage.isDirectory(pathinfo))
    		{
    			source = getEmptySource();	    			
    			stylesheet = getStylesheet(context, sessionContext, pathinfo, "attach", "");
    		}
    		else
    		{
            	source = buildError("AttachOnFile");
            	stylesheet = getStylesheet(context, sessionContext, pathinfo, "error", "");
    		}	    			    			    			    			    			           	          		        	      
		}
        else
        {
        	source = buildError("AttachOnNonExisting");
			stylesheet = getStylesheet(context, sessionContext, pathinfo, "error", "");
        }	
		
		return new TransformationResource(stylesheet, source);
	}
}

