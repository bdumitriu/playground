package gw.blog;

import gw.GwConstants;
import gw.GwContext;
import gw.render.ParserFactory;
import gw.render.ProxyStorage;
import gw.render.parsers.ParseException;
import gw.render.parsers.Parser;
import gw.storage.StorageException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.text.SimpleDateFormat;
import java.util.Date;


import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

/** 
 * XMLFactory contains all the XML operation needed for the entry/comment manipulations,
 * such as read, edit and save of the file    
 * 
 * @author Chris van Dam
 * @author Vincent Berkien
 * 
 */

public class XMLFactory
{

	/**
	 * Variables used to keep track of the current settings in the factory
	 */
	
	public Document document;
	private ProxyStorage proxyStorage;
	private String path;
	private Element root = null;
	
	/**
	 * Constructor
	 * 
	 * @param path
	 * 				Path to the file where the entry is stored
	 * @param storage
	 * 				The storage object
	 * @throws Exception
	 */
	public XMLFactory(String path, ProxyStorage proxyStorage)
            throws XMLFactoryException, StorageException {
		this.root = null;
		this.path = path;
		this.proxyStorage = proxyStorage;
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder;
        try {
            builder = factory.newDocumentBuilder();
            document = builder.parse(proxyStorage.getFile(path));
        } catch (ParserConfigurationException tce) {
            throw new XMLFactoryException(tce);
        } catch (SAXException te) {
            throw new XMLFactoryException(te);
        } catch (IOException ioe) {
            throw new XMLFactoryException(ioe);
        }
	}
    
    public void saveVirtualFile(String contentType) throws XMLFactoryException, StorageException
    {
        ByteArrayInputStream inStream = null;
        try {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer transformer = tFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            
            ByteArrayOutputStream outStream = new ByteArrayOutputStream();
            DOMSource source = new DOMSource(document);
            StreamResult result = new StreamResult(outStream);
            transformer.transform(source, result);
            
            inStream = new ByteArrayInputStream(outStream.toByteArray());
        } catch (TransformerConfigurationException tce) {
            throw new XMLFactoryException(tce);
        } catch (TransformerException te) {
            throw new XMLFactoryException(te);
        }

        HashMap<String, String> propertyMap = new HashMap<String, String>();
        propertyMap.put("content-type", contentType);
        proxyStorage.addVirtualFile(path, inStream, propertyMap);
    }
	
	/**
	 * @return
	 * 				The root of the file
	 */
	public Element getRoot()
	{
		if (root == null)
		{
			root = (Element) document.getDocumentElement(); 
		}
		
		return root;
	}
	
	/**
	 * @param name
	 * 				The name of the Element searched for
	 * @param elem
	 * 				An ancestor of the Element searched for
	 * @return
	 * 				The element with the name or Null if none found
	 */
	public Element getFirstElementChildByName(String name,Element elem)
	{
		NodeList list = elem.getElementsByTagName(name);
		
		for (int i = 0; i < list.getLength(); i++)
		{
			if (list.item(i).getNodeType() == Node.ELEMENT_NODE)
			{
				return (Element) list.item(i);
			}
		}
		
		return null;
	}
	
	public Element[] getElementChilds(Element elem)
	{
		LinkedList<Element> elems = new LinkedList<Element>();
		
		NodeList list = elem.getChildNodes();
		
		for (int i=0; i < list.getLength(); i++)
		{
			Node n = list.item(i);
			if (n.getNodeType() == Node.ELEMENT_NODE)
			{
				elems.add((Element)n);
			}
			
		}
		
		Element[] returns = new Element[elems.size()];
		
		for (int k = 0 ; k < returns.length ; k++ )
		{
			returns[k] = elems.get(k);
		}
		
		return returns;
	}
	
	/**
	 * @param elem
	 * 				An ancestor of the Element searched for 
	 * @return
	 * 				First TextNode found
	 */
	public Text getFirstTextNodeChild(Element elem)
	{
		NodeList list = elem.getChildNodes();
		
		for (int i = 0; i < list.getLength(); i++)
		{
			if (list.item(i).getNodeType() == Node.TEXT_NODE)
			{
				return (Text) list.item(i);
			}
		}
		
		return null;
	}
	
	/**
	 * Created an XMLtag with certain text 
	 * 
	 * @param name
	 * 				The name of the tag
	 * @param content
	 * 				The value in the tag
	 * @param parent
	 * 				The parent where you want to hang the tag 
	 */
	public void addTextElemAndAppend(String name,String content, Element parent)
	{
		Element elem = document.createElement(name);
		elem.appendChild(document.createTextNode(content));
		
        Element old = this.getFirstElementChildByName(name,parent);
        
		if (null != old )
		{
			parent.replaceChild( elem, old );
		}
		else
		{
			parent.appendChild(elem);
		}
	}
	
	/**
	 * @param name
	 * 				The name of the element you want to add
	 * @param parent
	 * 				The parent where you want to hang the element
	 * @return
	 * 				The element added
	 */
	public Element addElem(String name, Element parent)
	{
		Element elem = document.createElement(name);
		parent.appendChild(elem);
		return elem;
	}
	
	/**
	 * Add an attribute to an Element
	 * 
	 * @param name
	 * 				Name of the attribute 
	 * @param value
	 * 				Value given to the attribute
	 * @param elem
	 * 				The Element that gets the attribute
	 */
	public void addAttribute(String name, String value, Element elem)
	{
		elem.setAttribute(name, value);
	}
    
    public Node parseContent(GwContext context, String content, String contentType)
            throws XMLFactoryException {
        ParserFactory factory = context.getParserFactory();
        Parser parser = factory.lookupByType(contentType, GwConstants.XML_MIME_TYPE);
        org.jdom.Element result;
        try {
            result = (org.jdom.Element) parser.parse(content);
        } catch (ParseException pe) {
            throw new XMLFactoryException(pe);
        }
        
        Node domNode = BlogUtils.toDomNode((org.jdom.Element) result.detach());
        return document.importNode(domNode, true);
    }
    
    public static void createVirtualFile(ProxyStorage proxyStorage, String pathInfo, String fileInput, String contentType)
            throws StorageException {
        ByteArrayInputStream textStream = new ByteArrayInputStream(fileInput.getBytes());
        
        HashMap<String, String> propertyMap = new HashMap<String, String>();
        propertyMap.put("content-type", contentType);
        proxyStorage.addVirtualFile(pathInfo, textStream, propertyMap);
    }
    
    public static String formatDate(Date date) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        return sdf.format(date);
    }
}
