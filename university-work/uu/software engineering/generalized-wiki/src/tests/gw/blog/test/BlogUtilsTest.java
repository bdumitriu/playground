package gw.blog.test;

import gw.blog.BlogUtils;
import gw.storage.Storage;

import java.io.StringReader;
import java.util.HashSet;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Result;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamSource;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class BlogUtilsTest extends TestCase {

	Storage storage;
	
	public static void main(String[] args) {
		junit.textui.TestRunner.run(BlogUtilsTest.class);
	}

	protected void setUp() throws Exception {
		storage = new MockBlogStorage();
	}

	protected void tearDown() throws Exception {
		storage = null;
	}
	
	/**
     *	 Test the function uniqueId() by generating 25 IDs and comparing them
     */
	public void testUniqueID(){
		Set<String> set = new HashSet<String>();
		
		for (int i=0; i < 25; i++)
		{
			assertTrue(set.add(BlogUtils.uniqueId()));
		}
	}

	/**
     *	 Test the function mergeComments() by merging some fake comments and comparing them
     */
	public void testmergeComments(){
		Node node = BlogUtils.mergeComments("/fakePath/moreFake/com/",storage);
		
		assertNotNull("Node is null",node);
		
		NodeList list = node.getChildNodes();
		
		assertEquals("node list not complete",3,list.getLength());
		
		Node n1 = list.item(0);
		Node n2 = list.item(1);
		Node n3 = list.item(2);
		
		assertTrue("Node found were null", n1 != null && n2 != null && n3 != null);
		
		assertEquals("Node1 not the same",n1.getFirstChild().getFirstChild().getTextContent(),"GWRules");
		assertEquals("Node2 not the same",n2.getFirstChild().getFirstChild().getTextContent(),"qwerty");
		assertEquals("Node3 not the same",n3.getFirstChild().getFirstChild().getTextContent(),"Name3");
	}
	
	/**
     *	 Test the function mergeEntries() by merging some fake entries and comparing them
     */
	public void testmergeEntries(){
		Node node = BlogUtils.mergeEntries("/fakePath/moreFake/entry/",storage);
		
		assertNotNull("Node is null",node);
		
		NodeList list = node.getChildNodes();
		
		assertEquals("node list not complete",4,list.getLength());
		
		Node n1 = list.item(0);
		Node n2 = list.item(1);
		Node n3 = list.item(2);
		Node n4 = list.item(3);
		
		assertTrue("Node found were null", n1 != null && n2 != null && n3 != null && n4 != null);
		
		assertEquals("Node1 not the same",n1.getFirstChild().getFirstChild().getTextContent(),"Entry1");
		assertEquals("Node2 not the same",n2.getFirstChild().getFirstChild().getTextContent(),"Entry2");
		assertEquals("Node3 not the same",n3.getFirstChild().getFirstChild().getTextContent(),"Entry3");
		assertEquals("Node3 not the same",n4.getFirstChild().getFirstChild().getTextContent(),"Entry4GWRULEZZZ");
	}
    
    public void testXmlToString() throws TransformerException, ParserConfigurationException {
        String inputXml = "<test><title>Test blog</title><description>Blog om te testen</description></test>";
        StringReader input = new StringReader(inputXml);
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();
        
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        Source source = new StreamSource(input);
        Result result = new DOMResult(document);
        transformer.transform(source, result);
        
        String outputXml = BlogUtils.xmlToString(document.getChildNodes());
        assertEquals("Input xml must match output xml", inputXml, outputXml);
    }
}
