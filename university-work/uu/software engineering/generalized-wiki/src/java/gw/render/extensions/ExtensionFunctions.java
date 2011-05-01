package gw.render.extensions;

import gw.GwContext;
import gw.GwSessionContext;
import gw.Local;
import gw.ServletUtilities;

import gw.render.StylesheetApplyException;
import gw.render.StylesheetCreateException;
import gw.render.URIResolverImpl;
import gw.render.parsers.ParseException;
import gw.render.parsers.Parser;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.storage.StorageStatusMessage;

import java.io.IOException;
import java.io.InputStream;
import java.text.DateFormat;
import java.util.StringTokenizer;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.output.DOMOutputter;
import org.w3c.dom.Node;

import gw.users.acl.StorageACLAdapter;
import gw.users.acl.GwACLPermissions;

public class ExtensionFunctions {
	//private final static Namespace gwmlns = Namespace.getNamespace("http://www.cs.uu.nl/wiki/Gw");
	
	/**
     * This function enables xslt to get the content of a file changed by a parser.
     * 
     * @param pathinfo The file for which you want to get the content
     * @param action The parser to use specified by the inputContentType and ouptutContentType
     * @return The parsed content
     */
	public static String include(String pathinfo, String action){
		String returnValue = "";
		GwContext _context = Local.getGwContext();
		StringTokenizer st = new StringTokenizer(action,":");
        Parser parser = _context.getParserFactory().lookupByType(st.nextToken(),st.nextToken());
        //System.out.println("parser: "+parser);
        try{
            String content = ExtensionFunctions.getFileContent(pathinfo);
            returnValue = (String) parser.parse(content);
            //System.out.println("returnValue: "+returnValue);
        }catch(ParseException ex){
            returnValue = "Could not be parsed";

        }
        
        return returnValue;
    }
    
    /**
     * Prints a string in a gwml-page. For that purpose it replaces elements which are unparsable
     * using xslt 1.0 to parsable elements. 
     * @param content The string which is to be modified. 
     * @return String The modified string. 
     */
    //FIXME: this function is not working right now, because the string which is feeded to this 
    //function is not recognised as a string. Function will probably become obsolete when xslt 2.0 is used!!!
	public static String escapeTags(String content) {
          content = content.replaceAll(""+'&',"&amp;");
          content = content.replaceAll(""+'<',"&lt;");
          content = content.replaceAll(""+'>',"&gt;");
          return content; 
    }

    /**
     * Prints the content of a file onto a page, this is used to print xml-code in html
     * @param pathinfo The location of the file which is to be included
     * @return String The content of the file. 
     */
    public static String rawInclude(String pathinfo) {
        String result = getFileContent(pathinfo);
        //FIXME: this is a hack! Deletes all \n in a file. 
        result = result.replaceAll(""+(char)10, "");
        return result;
    }
       
    /**
     * Collects one or multiple files which are to be included in another gwml-file. 
     * The included file should be validated gwml. 
     * @param fileName The name of the file or the name of the path where the file(s) are which 
     * are to be included
     * @return Node A node (which actually consists of multiple nodes) which is the included document. 
     */
    public static Node wildInclude(String fileName){
        /* FIXME: this functions works fine on a single filename, but not on a path or when using 
         * wildcards. 
         */
        GwSessionContext sessionContext = Local.getSessionContext();
        GwContext context = Local.getGwContext();
        URIResolverImpl resolver = new URIResolverImpl(sessionContext, context);
        try {
            Element parsedElem = resolver.getFileElement(fileName);
            Document parsedDoc = parsedElem.getDocument();
        
            DOMOutputter domOut = new DOMOutputter();
            org.w3c.dom.Document w3cDoc =  domOut.output(parsedDoc);
            return w3cDoc;

        } 
        catch (StorageException e) {  
            e.printStackTrace();
        } 
        catch (JDOMException e) {
            e.printStackTrace();
        } 
        catch (ParseException e){  
        	e.printStackTrace();        
        }
        catch (StylesheetCreateException e){  
        	e.printStackTrace();        
        }
        catch (StylesheetApplyException e){  
        	e.printStackTrace();        
        }
        return null;
    }
    
	/**
     * This function enables xslt to get the content of some file.
     * 
     * This extension function is used for example in 
     * stylesheets/lib/preview-form.xsl
     * 
     * @param path The file for which you want to get the content
     * @return The content
     */
	public static String getFileContent(String path)
	{			
		Storage storage = Local.getSessionContext().getSessionStorage();		
		String text        = "";							
		
		try
		{	        
			InputStream input = storage.getFile(path);
			
			if(input != null) {
			   text = ServletUtilities.read(input);	
			}										
		}
		catch (StorageException e)
		{
			e.printStackTrace();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		return text;				
	}
	
	
	/**
     * Just a extension function that exposes the getContentType method
     * of ServletUtilities to xsl transformations.
     * 
     * This extension function is used for example in 
     * stylesheets/lib/preview-form.xsl
     * 
     * @param path The file for which you want to know the content-type
     * @return The contenttype
     */
	public static String getContentType(String path)
	{	
		Storage storage = Local.getSessionContext().getSessionStorage();
		return ServletUtilities.getContentType(storage, path);	
	}
	
	public static String getUrl(String action, String path)
	{
		Storage storage = Local.getSessionContext().getSessionStorage();
		String url = "";
		try
		{
			url = ServletUtilities.normalizePathInfo(storage, getPrefix() + action + path);
		}
		catch (StorageException e)
		{
			e.printStackTrace();
		}
		return url;
	}
	
	public static String getPrefix()
	{
		return Local.getGwContext().getApplicationPrefix();
	}
	
	public static org.w3c.dom.Node getFileStatus(String fileName) 
	{
		Storage storage = Local.getSessionContext().getSessionStorage();
		GwContext context = Local.getGwContext();
						
        try {
        	
        	Element result = new Element("status");
        	StorageStatusMessage statusmsg = 
        		(StorageStatusMessage) storage.getStatus(fileName).get(fileName);
        	Element element = new Element("svnstatus");        	
			if(statusmsg != null)
 		   		element.setText(ServletUtilities.getStatusDescription(statusmsg, context));
        	result.addContent(element);
        	element = new Element("date");
			if(statusmsg != null) {
 		       	DateFormat dateFormat = (DateFormat) context.getConfigResources().getObject("Date.Format");
        		if (statusmsg.getLastChangedDate() != null)
        			element.setText(dateFormat.format(statusmsg.getLastChangedDate()));
			}
        	result.addContent(element);
        	element = new Element("author");        	
			if(statusmsg != null)
    			element.setText(statusmsg.getLastCommitAuthor());
        	result.addContent(element);
        	element = new Element("revision");        	
			if(statusmsg != null)
    			element.setText("" + statusmsg.getLastChangedRevision());
        	result.addContent(element);
        	
        	DOMOutputter domOut = new DOMOutputter();
        	org.w3c.dom.Document w3cDoc =  domOut.output(new Document(result));
        	return w3cDoc;

		} catch (StorageException e) {
			e.printStackTrace();			
		} catch (JDOMException e) {
			e.printStackTrace();			
		}	
		
		return null;
	}	
    
    public static String getAcl(String pathinfo) {
        Storage storage = Local.getSessionContext().getSessionStorage();
        StorageACLAdapter aclStorage = new StorageACLAdapter(pathinfo, storage);
        StringBuffer acl = new StringBuffer();
        for (int i = 0; i < GwACLPermissions.ALL_PERMISSIONS.length; i++) {
            try {
                String permission = aclStorage.getPermissionLine(GwACLPermissions.ALL_PERMISSIONS[i]);
                if (permission != null) {
                    acl.append(permission);
                }
            } catch (StorageException se) {
                se.printStackTrace();
            }

            if (i != GwACLPermissions.ALL_PERMISSIONS.length - 1) {
                acl.append(":");
            }
        }
        return acl.toString();
    }
    
}
