package gw.render.locator;



import gw.GwConstants;
import gw.GwContext;
import gw.GwSessionContext;
import gw.ServletUtilities;
import gw.render.parsers.ParseException;
import gw.render.Stylesheet;
import gw.render.StylesheetCreateException;
import gw.render.TransformationResource;
import gw.storage.Storage;
import gw.storage.StorageException;

import javax.xml.transform.Source;

import org.jdom.transform.JDOMSource;

public class EditLocator extends ActionResourceLocator{
	

	public TransformationResource locate(GwContext context, GwSessionContext sessionContext, String pathinfo) throws StylesheetCreateException, StorageException, ParseException
	{
		Storage storage = sessionContext.getSessionStorage();
        
    	String contentType = ServletUtilities.getContentType(storage, pathinfo);
    	
    	Stylesheet stylesheet;  
    	Source source;
    	
		if (storage.fileExists(pathinfo))
		{
			String svnContentType = (String) storage.getProperties(pathinfo).get("isAttachment");		
	        if (svnContentType != null && svnContentType.equals("true"))
	        {
	        	source = getEmptySource();
	        	stylesheet = getStylesheet(context, sessionContext, pathinfo, "attach", "");
	        }
	        else
	        {	    		
	    		if(storage.isDirectory(pathinfo))
	    		{
	    			source = new JDOMSource(parseDir(storage, storage.getDirListing(pathinfo), pathinfo));	    			
	    			stylesheet = getStylesheet(context, sessionContext, pathinfo, "view", "-dir");
	    		}
	    		else
	    		{	    			   
	    			source = new JDOMSource(buildXml(context, storage, contentType, pathinfo));
	    			stylesheet = getStylesheet(context, sessionContext, pathinfo, "edit", "_" + contentType);
	    		}	    			    			    			    			    			    			           	          
	        }
		}
        else

        {
            String newContentType = GwConstants.GWML_MIME_TYPE;
            String[] values = (String[]) sessionContext.getParameters().get("content-type");
            if (values != null)
                newContentType = values[0];
            
        	//source = getEmptySource();
        	source = new JDOMSource(buildXml(context, storage, newContentType, pathinfo));
			stylesheet = getStylesheet(context, sessionContext, pathinfo, "edit", "_" + newContentType);
        }	
		
		return new TransformationResource(stylesheet, source);
	}
	
	
}
