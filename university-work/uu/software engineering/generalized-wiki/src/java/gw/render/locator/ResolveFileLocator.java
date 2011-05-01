package gw.render.locator;

import javax.xml.transform.Source;

import org.jdom.transform.JDOMSource;

import gw.GwConstants;
import gw.GwContext;
import gw.GwSessionContext;
import gw.ServletUtilities;
import gw.render.parsers.ParseException;
import gw.render.parsers.Parser;
import gw.render.Stylesheet;
import gw.render.StylesheetCreateException;
import gw.render.StylesheetFactory;
import gw.render.TransformationResource;
import gw.storage.Storage;
import gw.storage.StorageException;

public class ResolveFileLocator extends ActionResourceLocator {

	public TransformationResource locate(GwContext context, GwSessionContext sessioncontext, String pathinfo) throws StylesheetCreateException, StorageException, ParseException 
	{		
		Storage storage = sessioncontext.getSessionStorage();	
    	String contentType = ServletUtilities.getContentType(storage, pathinfo);
    	
    	StylesheetFactory factory = context.getStylesheetFactory();
    	Stylesheet stylesheet = factory.newIdentity(context, sessioncontext);
    	Source source = null;    	             
	
		String svnContentType = (String) storage.getProperties(pathinfo).get("svn:mime-type");		
        if (svnContentType != null && svnContentType.equals("application/octet-stream"))
        {
        	throw new StorageException("Only xml allowed in URIResolver. File: " + pathinfo);
        }
        else
        {
    		if(storage.isDirectory(pathinfo))
    		{
    			source = new JDOMSource(parseDir(storage, storage.getDirListing(pathinfo), pathinfo));
    		}
    		else
    		{
    			Parser parser = context.getParserFactory().lookupByType(contentType,GwConstants.XML_MIME_TYPE);    			
    			source = new JDOMSource((org.jdom.Element) parser.parse(storage.getFile(pathinfo)));
    		}	    			    			    			   
        }	        	       	   				   		
		
		return new TransformationResource(stylesheet, source);
	}
}
