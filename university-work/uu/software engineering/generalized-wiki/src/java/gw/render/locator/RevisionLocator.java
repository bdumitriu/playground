package gw.render.locator;

import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;

import gw.GwContext;
import gw.GwSessionContext;
import gw.ServletUtilities;
import gw.render.parsers.ParseException;
import gw.render.Stylesheet;
import gw.render.StylesheetCreateException;
import gw.render.TransformationResource;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.storage.StorageLogMessage;

import javax.xml.transform.Source;

import org.jdom.Attribute;
import org.jdom.Element;
import org.jdom.transform.JDOMSource;

public class RevisionLocator extends ActionResourceLocator{
	

	public TransformationResource locate(GwContext context, GwSessionContext sessionContext, String pathinfo) throws StylesheetCreateException, StorageException, ParseException
	{
		Storage storage = sessionContext.getSessionStorage();				
    	String contentType = ServletUtilities.getContentType(storage, pathinfo);    	  
    	

    	
    	Stylesheet stylesheet;  
    	Source source;

		if (storage.fileExists(pathinfo))
		{		
			source = new JDOMSource(getLog(storage, pathinfo));
			stylesheet = getStylesheet(context, sessionContext, pathinfo, "revision", "_" + contentType);				    			    			    			    			    			           	          		        	     
		}
        else
        {
        	source = buildError("LogRequestedOnNonExistingPath");
			stylesheet = getStylesheet(context, sessionContext, pathinfo, "error", "");
        }	
		
		return new TransformationResource(stylesheet, source);  
	}
	
	private Element getLog(Storage storage, String pathinfo) throws StorageException
    {
    	TreeSet<StorageLogMessage> logs = new TreeSet<StorageLogMessage>(storage.getLog(pathinfo));
    	
    	Element log = new Element("log");
    	
    	SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
        Iterator logsiterator = logs.iterator();
        while (logsiterator.hasNext()) {
            StorageLogMessage logmsg = (StorageLogMessage) logsiterator.next();
           
	    	Element author = new Element("author");
	    	author.addContent(logmsg.getAuthor());
	    	
	    	String dateRepresentation = sdf.format(logmsg.getDate());
	    	Element date = new Element("date");
	    	date.addContent(dateRepresentation);

	    	Element logEntry = new Element("logEntry");
	    	logEntry.setAttribute(new Attribute("revision", String.valueOf(logmsg.getRevisionNumber())));
	    	logEntry.addContent(author);
	    	logEntry.addContent(date);
	    	
	    	if(logmsg.getChangedPaths().size() > 0)
	    	{
		    	Element paths = new Element("paths");
			    Iterator iter = logmsg.getChangedPaths().entrySet().iterator();
			    while(iter.hasNext()){
			   		Element path = new Element("path");
			    	Map.Entry<String, Character> pairs = (Map.Entry)iter.next();
			    	path.setAttribute(new Attribute("action", pairs.getValue().toString()));
			   		path.addContent(pairs.getKey());
			   		
			   		paths.addContent(path);
			   	}
			    
			    logEntry.addContent(paths);
	    	}
 
	    	Element msg = new Element("msg");
	    	msg.addContent(logmsg.getMessage());
	    	
	    	logEntry.addContent(msg);
	    	log.addContent(logEntry);
        }
    	
    	return log;
    }
}