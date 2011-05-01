package gw.blog;

import gw.GwConstants;
import gw.Local;
import gw.storage.InsufficientStorageAccessPermissionsException;
import gw.storage.Storage;
import gw.storage.StorageException;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.DOMOutputter;

public final class BlogUtils {

    private BlogUtils() {
        // No instantiation
    }

    public static String uniqueId() {
   	
    	Random rnd2 = new Random(); 
    	long seed = rnd2.nextLong();
    	
    	Random rnd = new Random(seed);
    	
    	return (Long.toString(System.currentTimeMillis()) + Long.toString(Math.abs(rnd.nextLong())));
    	
    }
    
    
    public static org.w3c.dom.Node mergeEntries(String path)  {
        Storage storage = Local.getSessionContext().getSessionStorage();
        return mergeEntries(path,storage);
    }
    
    public static org.w3c.dom.Node mergeEntries(String path, Storage storage)
    {
    	Element dir = new Element("dir");
        try {
            Iterator<String> iter = storage.getDirListing(path);
            while (iter.hasNext()) {
                String dirname = iter.next();
                String filename = dirname + "/entry.xml";
                
                if (storage.isDirectory(dirname) && storage.fileExists(filename)) {
                    dir.addContent(parseFile(storage, filename));
                }
            }
        } catch (StorageException se) {
            se.printStackTrace();
        }
        
        return toDomNode(dir);
    }


    public static org.w3c.dom.Node mergeComments(String path) {
        Storage storage = Local.getSessionContext().getSessionStorage();
        return mergeComments(path,storage);
    }
    
    public static org.w3c.dom.Node mergeComments(String path, Storage storage)
    {
        Element dir = new Element("dir");
        try {
            Iterator<String> iter = storage.getDirListing(path);
            while (iter.hasNext()) {
                String filename = iter.next();
                Map<String, String> properties = storage.getProperties(filename);
                if (GwConstants.BLOG_COMMENT_MIME_TYPE.equals(properties.get("content-type"))) {
                    dir.addContent(parseFile(storage, filename));
                }
            }
        } catch (StorageException se) {
            se.printStackTrace();
        }

        return toDomNode(dir);
    }
    
    private static Element parseFile(Storage storage, String filename) {
        Element file = new Element("file");
        file.setAttribute("path", filename);
        try {
            SAXBuilder builder = new SAXBuilder();
            Element content = builder.build(storage.getFile(filename)).detachRootElement();
            file.addContent(content);
        } catch (InsufficientStorageAccessPermissionsException isape) {
            /* Ignore, entry is saved as a draft */
        } catch (StorageException se) {
            se.printStackTrace();
        } catch (JDOMException jde) {
            jde.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }

        return file;
    }
    
    public static org.w3c.dom.Node toDomNode(Element root) {
        org.w3c.dom.Node domNode = null;
        try {
            Document document = new Document(root);
            DOMOutputter outputter = new DOMOutputter();
            domNode = outputter.output(document).getDocumentElement();
        } catch (JDOMException jde) {
            jde.printStackTrace();
        }
        
        return domNode;
    }
    
    public static String xmlToString(org.w3c.dom.NodeList nodeList) {
        StringWriter output = new StringWriter();
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer();
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            Result result = new StreamResult(output);
            for (int i = 0; i < nodeList.getLength(); i++) {
                Source source = new DOMSource(nodeList.item(i));
                transformer.transform(source, result);
            }
        } catch (TransformerConfigurationException tce) {
            tce.printStackTrace();
        } catch (TransformerException te) {
            te.printStackTrace();
        }

        return output.toString();
    }

}
