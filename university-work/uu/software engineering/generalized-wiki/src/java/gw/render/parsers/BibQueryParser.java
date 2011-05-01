package gw.render.parsers;

import gw.render.parsers.ParseException;
import gw.render.parsers.Parser;
import gw.render.URIResolverImpl;
import gw.query.*;
import gw.Local;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.List;
import java.util.Iterator;
import java.util.StringTokenizer;

import org.jdom.Element;
import org.jdom.Document;
import org.jdom.Attribute;
import org.jdom.input.SAXBuilder;
import org.jdom.transform.JDOMSource;
import org.jdom.xpath.XPath;
import javax.xml.transform.TransformerException;
import javax.xml.transform.Source;
import gw.render.TransformationResource;

import gw.GwConstants;
import gw.GwSessionContext;
import gw.GwContext;
import org.jdom.input.SAXBuilder;

import gw.storage.Storage;

public class BibQueryParser implements Parser {

	private static BibQueryParser bibQueryParser;
    
    /**
     * @return The instance of this parser 
     */
	public static synchronized BibQueryParser getInstance(){
    	if (bibQueryParser==null) bibQueryParser = new BibQueryParser();
		return bibQueryParser;
    }

    /**
     * Parse the given Bibtex-query content
     *  
     * @param reader
     *            The Reader that contains the input to parse
     * @return Element The parsed input
     * @throws ParseExceptionIterator iter=nodeList.iterator();
     */

    private GwSessionContext _sessioncontext;
    private GwContext _context;
    private Storage _storage;
    
    public Object parse(Reader reader) throws ParseException {
        _sessioncontext = Local.getSessionContext();
        _context = Local.getGwContext();
        _storage = Local.getSessionContext().getSessionStorage();
        
        List result = null;
        Document mergedDocument = new Document();
        Element el =  new Element("Entries");

        try {
            Document document = new SAXBuilder().build(reader);
            
            // Get the query from the input document (optional)
            System.out.println("getting query...");
            String queryString = getBibtexQuery(document);
            
            // Get the specified sorting order from the document (optional)
            System.out.println("getting order");
            Element order = getBibtexSortingOrder(document);
            
            // Get the includes from the document (required attribute!!)
            System.out.println("getting include list");
            List includeList =  XPath.selectNodes(document, 
            "/document/include");
            
            // parse the BibtexQuery to an XPath expression/query
            String XPathquery = "//tuple";
            if (!queryString.equals("")) { 
                QNode queryTree = BibTexQuery.parseQuery( queryString ); // parse
                XPathquery = QueryMapper.getRootQuery(queryTree); // pretty-print
            }
            System.out.println("XPathquery" + XPathquery);
            
            // Process the includes
            mergedDocument = mergeDocuments(includeList);
            
            // Apply the XPath query to the merged document
            result =  XPath.selectNodes( mergedDocument, XPathquery );
            System.out.println("Aantal childeren in result: "+result.size());
            
            // Parent element for the elements that are queried by the XPath query
            Element listElement = new Element("list");
            
            // Detatch all the elements so that they have no parent anymore.
            for (int i = 0; i < result.size(); i++) {
                ((Element)result.get(i)).detach();
            }
            
            // Make the <Entries><list><tuple></tuple></list></Entries> structure 
            listElement.addContent(result);
            listElement.detach();
            el.addContent(listElement);
            
            // Add the sort-order-element to be used in the XSLT process
            System.out.println("order: " + order.toString() + ", " + order.getName() + ", " + order.getValue());
            el.addContent(order);
        
            /* 
<document>
<sort>year</sort>

<include>/trunk/BibtexPage3</include>

</document>
             */
            
        } catch (Exception e) {
            System.out.println("ERROR!");            
            throw new ParseException(e);
        }
        System.out.println("return root:" +el);  
        return el;
    }
    
    private Document mergeDocuments ( List includeList ){
        System.out.println("MergeDocumnets --> Start");
        Document md = new Document();
        Element el = new Element("Entries");
        
        System.out.println("include list size: " + includeList.size());
        
        for(int i =0; i < includeList.size(); i++) {
            System.out.println("Element: " + ((Element)includeList.get(i)).getText());
            Element tmpResult = include( ((Element)includeList.get(i)).getText() );
            tmpResult.detach();
            el.addContent(tmpResult);
        }  
        md.setRootElement(el);
        
        return md;
    } 

    /**
     * Parse the given XML content
     * 
     * @param inputStream
     *            The InputStream that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(InputStream inputStream) throws ParseException {
        return parse(new InputStreamReader(inputStream));

    }
    
    /**
     * Parse the given XML content
     * 
     * @param string
     *            The String that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(String string) throws ParseException {
        return parse(new StringReader(string));
    }
    
    private Element include(String uri) {
           
        // Step 1: Determine the mime-type from that file
        System.out.println("Start of the include...");
        System.out.println("Page :" + uri);
        String contentType = null;
        try
        {
            System.out.println("Storage: "+ _storage);
            contentType = (String) _storage.getProperties(uri).get("content-type");
            System.out.println("Include ==> contentType: " + contentType);
        }
        catch(Exception e){
            System.out.println("Sorry! Couldn't get the contentType");
        }        
        
        // Step 2: Apply the parser to the file
        Parser parser = _context.getParserFactory().lookupByType(contentType, GwConstants.XML_MIME_TYPE); 
        
        // Step 3: you get the XML as return value
        Element elem = null;
        try{
            elem = new SAXBuilder().build(new StringReader("<element/>")).getRootElement();
            elem = (Element)parser.parse( _storage.getFile(uri) );
            System.out.println("include --> element: " + elem);
        }catch(Exception e){
            System.out.print("Sorry! Couldn't parse the file");              
        }                          
        return elem;
    }
    
    public String getBibtexQuery(Document document){
        String result;
        
        try {
            Element query = (Element) XPath.selectSingleNode(
                    document,
                   "/document/query" );
            System.out.println("one query");
                   
            result = query.getText();
        }
        catch( Exception e) {
            //no query available
            System.out.println("NO QUERY");
            result = "";
        }
        return result;
    }
    
    public Element getBibtexSortingOrder(Document document) {
        Element result;
        try {
            result = (Element) XPath.selectSingleNode(document, 
                     "/document/sort");
            result.detach();
            
        }
        catch( Exception e) {
            //no sorting option specified
            result = new Element("sort");
            System.out.println("NO SORT");
            //result = new Element("sort");
        }
        return result;
    }
}

