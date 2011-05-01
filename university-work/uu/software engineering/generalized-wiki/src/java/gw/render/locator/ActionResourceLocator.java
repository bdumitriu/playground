package gw.render.locator;

import gw.GwConstants;
import gw.GwContext;
import gw.GwSessionContext;
import gw.ServletUtilities;
import gw.render.Stylesheet;
import gw.render.StylesheetCreateException;
import gw.render.StylesheetFactory;
import gw.render.parsers.Parser;
import gw.render.parsers.ParseException;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.storage.StorageStatusMessage;
import gw.render.extensions.ExtensionFunctions;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Date;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.jdom.Element;
import org.jdom.transform.JDOMSource;

public abstract class ActionResourceLocator implements ResourceLocator {
		
	protected Stylesheet getStylesheet(GwContext context, GwSessionContext sessioncontext, String pathinfo, String head, String tail) throws StorageException, StylesheetCreateException
    {
		Storage storage = sessioncontext.getSessionStorage();
    	String xslbasename = checkFileName(head + tail);
    	String xslpath = locateClosestFile(storage, pathinfo, xslbasename + ".xsl", "stylesheets/");
		
		InputStream stream;
        if(xslpath != null) 
        {	            
           stream = storage.getFile(xslpath);
        }
        else
        {
        	context.log("No XSL stylesheet " + xslbasename + ".xsl in repository for " + pathinfo + ". Falling back to default stylesheet.");
            stream = getClass().getClassLoader().getResourceAsStream("/gw/stylesheets/" + xslbasename + ".xsl");	            	
        }
        
        if(stream == null) 
        {
        	context.log("No default XSL stylesheet " + xslbasename + ".xsl available for " + pathinfo);	 	                
           stream = getClass().getClassLoader().getResourceAsStream("/gw/stylesheets/" + head  + "_default.xsl");	                	                
        }

        StylesheetFactory factory = context.getStylesheetFactory();
        return factory.newByType(GwConstants.XSLT_MIME_TYPE, stream, context, sessioncontext);    	
    }
    
    
    
    /**
     * This function generates a directory listing.
     * 
     * @param dirs
     *            the elements in the baseDir
     * @param baseDir
     *            base directory
     * @return gwml tree with link elements
     * @throws StorageException
     */
	protected Element parseDir(Storage storage, Iterator<String> dirs, String baseDir) throws StorageException {
        Element element, root, section, title;
        
        List<Element> dirElements = new ArrayList<Element>();
        List<Element> fileElements = new ArrayList<Element>();
        
        root = new Element("document");
        section = new Element("section");
        title = new Element("title");
        title.addContent(baseDir);
        section.addContent(title);
          
        while (dirs.hasNext()) {
            String entry = dirs.next();
            element = new Element("link");
            if (storage.isDirectory(entry)) {              
                element.setAttribute("target", entry);
                dirElements.add(element);
            } else {
                element.setAttribute("target", entry);
                fileElements.add(element);
            }
        }
        section.addContent(dirElements);
        section.addContent(fileElements);
        
        root.addContent(section);
        return root;
    }
    
    protected Source getEmptySource()
    {
    	// This should be a new object, because the stream can only be closed one time.
    	return new StreamSource(new StringReader("<empty/>"));
    }
    
    protected Source buildError(String errorIdentifier)
    {
    	Element root = new Element("error");
    	
    	Element identifier = new Element("identifier");
    	identifier.addContent(errorIdentifier);
    	
    	root.addContent(identifier);
        return new JDOMSource(root);
    }
    
    
    protected Element getCommitData(GwContext context, Storage storage, String pathInfo) throws StorageException
    {

        Element root = new Element("committingFiles");
        
        
        Iterator conflicts = storage.getConflicts();
        Element conflict = getConflicts(conflicts, context);
        root.addContent(conflict);
        
        // fileStatuses
        Map fileMap = storage.getStatus(pathInfo, true);
        if (!fileMap.isEmpty()) {
        	Element commitEntry = formatStatuses(fileMap, true, storage, "/gw", context);
    		root.addContent(commitEntry);
        }
        
        return root;
  
    }
    
    /**
     * Print the encountered conflicts.
     * 
     * @param conflicts
     *            An Iterator containing the encountered conflicts
     * @param request
     *            The corresponding servlet request
     * @return String representing all encountered conflicts
     */
    protected Element getConflicts(Iterator conflicts, GwContext context) {
    	Element conflictElems = new Element("mergeConflicts");
    	
    	Element contextPath = new Element("contextPath");
    	contextPath.addContent("/gw");
    	conflictElems.addContent(contextPath);

            while (conflicts.hasNext()) {
            	Element conflict = new Element("conflict");
            	Element fileName = new Element("fileName");
            	fileName.addContent((String) conflicts.next());
            	conflict.addContent(fileName);
            	conflictElems.addContent(conflict);
            }

            return conflictElems;
            

        }
  
    
    /**
     * This method creates a String, containing HTML. The given info of the
     * given stats are formatted in a HTML-table. The boolean parameter
     * indicates if the Files that aren't changed have to be in the table too.
     * 
     * @param stats
     *            The array with Status objects
     * @param changedOnly
     *            If true, only the changed files appear. Otherwise all files.
     * @return The String containing the HTML-table
     * @throws StorageException
     */
    protected Element formatStatuses(Map stats, boolean changedOnly, Storage storage,
            String contextpath, GwContext context) throws StorageException {
      
        Element root = new Element("fileStatuses");

        Iterator statsmsgs = stats.entrySet().iterator();
        while (statsmsgs.hasNext())
        {
        	Map.Entry entry = (Map.Entry) statsmsgs.next();
        	String link = (String) entry.getKey();
        	StorageStatusMessage storeStatmsg = (StorageStatusMessage) entry.getValue();

            String statusdescr = ServletUtilities.getStatusDescription(storeStatmsg, context);
            String lastrev = storeStatmsg.getLastChangedRevision() + "";
            
            if ( (changedOnly && !statusdescr.equals(context.getTextResources().getString("Status.Normal")))
                 || !changedOnly)
            {
	        	Element status = new Element("status");
	        	
	        	Element fileName = new Element("filename");
	        	fileName.addContent(link);
	        	status.addContent(fileName);
	        	
	        	String isDir = "";
	        	if(storeStatmsg.isDirectory())
	        		isDir = "D";
	        	else
	        		isDir = "F";
	        	
	        	Element dirOrFile = new Element("dirOrFile");
	        	dirOrFile.addContent(isDir);
	        	status.addContent(dirOrFile);
	        	
	        	Element lastChanged = new Element("lastChangedDate");
	            Date date = storeStatmsg.getLastChangedDate();
	            if (date != null)
	            	lastChanged.addContent(date.toString());
	            else
	            	lastChanged.addContent("");
	        	status.addContent(lastChanged);
	        	
	        	Element lastAuthor = new Element("lastCommitAuthor");
	        	lastAuthor.addContent(storeStatmsg.getLastCommitAuthor());
	        	status.addContent(lastAuthor);
	        	
	        	Element statusDescription = new Element("statusDescription");
	        	statusDescription.addContent(statusdescr);
	        	status.addContent(statusDescription);
	        	
	        	Element lastRevisionNr = new Element("lastRevisionNr");
	        	lastRevisionNr.addContent(lastrev);
	        	status.addContent(lastRevisionNr);
	            
	            root.addContent(status);
            }
        }
        
        return root;
    }

  

    //(storage, pathinfo, "view_application-xhtml-xml.xsl", "stylesheets/")
    protected static String locateClosestFile(Storage storage, String start_path, String file,
            String priority_dir) throws StorageException {
        String[] path_part = ServletUtilities.splitPathInArray(start_path);
        for (int i = path_part.length - 1; i >= 0; i--) {
            String cur_path = "/" + ServletUtilities.joinPathFromArray(path_part, i);
            if (storage.fileExists(cur_path + priority_dir + file))
                return cur_path + priority_dir + file;
        }
        if (storage.fileExists("/" + priority_dir + file))
            return "/" + priority_dir + file;
        return null;
    }
    
    protected String checkFileName(String name){
    	return name.replaceAll("\\W", "-");
    }
    
    protected Element buildXml(GwContext context, Storage storage, String contentType, String pathInfo) throws StorageException, ParseException
	{	
		String content = "";
		Element parse = new Element("empty");
		
		try
		{
			if(storage.fileExists(pathInfo)) {
				
				Parser parser = context.getParserFactory().lookupByType(contentType, GwConstants.XML_MIME_TYPE);
				parse = (org.jdom.Element) parser.parse(storage.getFile(pathInfo));
				
				InputStream input = storage.getFile(pathInfo);
				if(input != null)
				{
					content = ServletUtilities.read(input);				 									
			   		content = content.replaceAll("\\r\\n","\n");
				}

			}
		}
		catch(IOException ioe){}
			
		Element actionPathElement = new Element("actionPath");
		actionPathElement.setText(context.getApplicationPrefix() + "preview" + pathInfo);
		
		Element contentElement = new Element("content");
		contentElement.setText(content);
		
		parse.detach();
		Element parseContentElement = new Element("parsecontent");
		parseContentElement.addContent(parse);
			
		Element contentTypeValueElement = new Element("contentTypeValue");
		contentTypeValueElement.setText(contentType);
		
		Element element = new Element("editForm");
		element.addContent(actionPathElement);
		element.addContent(contentElement);
		element.addContent(parseContentElement);
		element.addContent(contentTypeValueElement);
		
		return element;
	}
}
