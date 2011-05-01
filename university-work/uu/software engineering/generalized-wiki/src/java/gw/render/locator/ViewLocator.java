package gw.render.locator;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.jdom.transform.JDOMSource;

import gw.GwConstants;
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

public class ViewLocator extends ActionResourceLocator{
	

	public TransformationResource locate(GwContext context, GwSessionContext sessionContext, String path) throws StylesheetCreateException, StorageException, ParseException
	{
		Storage storage = sessionContext.getSessionStorage();
		
		
		String pathinfo = path;
    	String contentType = ServletUtilities.getContentType(storage, path);
    	
    	Stylesheet stylesheet;
    	Source source;
    	
        
        
		if (storage.fileExists(pathinfo))
		{
			String svnContentType = (String) storage.getProperties(pathinfo).get("isAttachment");		
	        if (svnContentType != null && svnContentType.equals("true"))
	        {
	        	StylesheetFactory factory = context.getStylesheetFactory();
	        	stylesheet = factory.newIdentity(context, sessionContext);
	        	source = new StreamSource(storage.getFile(pathinfo));
	        }
	        else
	        {
	    		String head = "view";
	    		String tail;
	    		if(storage.isDirectory(pathinfo))
	    		{
                source = new JDOMSource(parseDir(storage, storage.getDirListing(pathinfo), "/"/*pathinfo*/));
	    			tail = "-dir";
	    		}
	    		else
	    		{
	    			Parser parser = context.getParserFactory().lookupByType(contentType, GwConstants.XML_MIME_TYPE);
	    			source = new JDOMSource((org.jdom.Element) parser.parse(storage.getFile(pathinfo)));	    				   
	    			tail = "_" + contentType;
	    		}
	    			    			    		
	    		stylesheet = getStylesheet(context, sessionContext, pathinfo, head, tail);
	        }	        	       
		}
        else
        {
        	source = new JDOMSource(buildXml(context, storage, contentType, pathinfo));    				   
			stylesheet = getStylesheet(context, sessionContext, pathinfo, "edit", "_" + contentType);
        }	
		
		return new TransformationResource(stylesheet, source);
	}
}
