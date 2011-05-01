package gw.blog.test;

import gw.blog.XMLFactory;
import gw.blog.XMLFactoryException;
import gw.render.ProxyStorage;
import gw.storage.StorageException;

import java.util.GregorianCalendar;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class XMLFactoryTest extends TestCase {
	
	private XMLFactory factory;
	private ProxyStorage proxyStorage;
     /**
     * Construct a new Testcase with the given name
     * @param name
     */
    public XMLFactoryTest(final String name) {
        super(name);
    }
    
    /**
     * Run this testcase in textmode
     * @param args
     */
    public static void main( String[] args ) {
        junit.textui.TestRunner.run(suite());
    }
    
    /**
     * Create a TestSuite based on this TestCase
     * @return the generated TestSuite
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(XMLFactoryTest.class);
        return suite;
    }

    /**
     * Initialize for each test
     * @throws StorageException 
     * @throws XMLFactoryException 
     */ 
    protected void setUp() throws XMLFactoryException, StorageException {
    	proxyStorage = new ProxyStorage(new MockBlogStorage());
    	
   		factory = new XMLFactory("/testPath/xmlfactTest.xml",proxyStorage);
	}

    /**
     * Destruct for each test
     */
    protected void tearDown() {
    	factory = null;
    }

 
    /**
     * Test the function getRoot()
     */
    public void testgetRoot() throws Exception{
    	assertEquals("root not same root as factory",factory.document.getDocumentElement(),factory.getRoot());
    	assertEquals("root not same name",factory.getRoot().getNodeName(),"entry");
    }
    
    /**
     * Test the function addTextElemAndAppend()
     * 
     * 1. add a new element 'test' to the root
     * 2. check if element is created
     * 3. substitute the element
     * 4. check if substitution was correct 
     * 
     */
    public void testaddTextElemAndAppend() throws Exception{
    	Element root = factory.getRoot();
    	int templength = root.getChildNodes().getLength();
    	
    	factory.addTextElemAndAppend("test","text",root);
    	assertTrue("length Childnodes root not increased",root.getChildNodes().getLength() == templength +1);
    	
    	NodeList list = root.getChildNodes();
    	boolean found = false;
    	
    	for (int i=0; i < templength+1; i++){
    		Element temp = (Element) list.item(i);
    		if (temp.getNodeName().equals("test")){
    			for (int j=0; j < temp.getChildNodes().getLength(); j++){
    	    		Node newtemp = temp.getChildNodes().item(j);
    	    		if (newtemp.getNodeValue().equals("text")){
    	    		   	found = true;
    	    		   	break;
    	    		}
    	    	} 	
    		}
    	}
    	
    	assertTrue("node not added by function",found);
    	factory.addTextElemAndAppend("test","hello there",root);
    	assertTrue("length Childnodes root not increased",root.getChildNodes().getLength() == templength +1);
    	found = false;
    	
    	for (int i=0; i < templength+1; i++){
    		Element temp = (Element) list.item(i);
    		    		
    		if (temp.getNodeName().equals("test")){
    			for (int j=0; j < temp.getChildNodes().getLength(); j++){
    	    		Node newtemp = temp.getChildNodes().item(j);
    	    		if (newtemp.getNodeValue().equals("hello there")){
    	    		   	found = true;
    	    		   	break;
    	    		}
    	    	} 	
    		}
    	}
    	assertTrue("node not replaced by function",found);
    }
    
    /**
     *	 Test the function testaddElem()
     */
    public void testaddElem() throws Exception{
    	boolean found = false;
    	Element root = factory.getRoot();
    	factory.addElem("testaddelem",root);
    	
    	for (int j=0; j < root.getChildNodes().getLength(); j++){
    		Element newtemp = (Element) root.getChildNodes().item(j);
    		if (newtemp.getNodeName().equals("testaddelem")){
    		   	found = true;
    		   	break;
    		}
    	}
    	assertTrue("Node failed to add",found);
    }
    
    /**
     *	 Test the function addAttribute()
     */
    public void testaddAttribute() throws Exception{
    	Element root = factory.getRoot();
    	factory.addAttribute("testatt","testvalue",root);
    	assertEquals("attribute not the same","testvalue",root.getAttribute("testatt"));
    }
 
    /**
     *   Test the function getFirstElementChildByName()
     */
	public void testgetFirstElementChildByName(){
		Element root = factory.getRoot();
		assertNotNull("root not found",root);
		
		assertEquals("content not same",factory.getFirstElementChildByName("content",root).getNodeName(),("content"));
		assertEquals("title not same",factory.getFirstElementChildByName("title",root).getNodeName(),("title"));
		assertEquals("date not same",factory.getFirstElementChildByName("date",root).getNodeName(),("date"));
		assertEquals("author not same",factory.getFirstElementChildByName("author",root).getNodeName(),("author"));
		assertEquals(factory.getFirstElementChildByName("me",root),null);
	}
	
	/**
     *   Test the function getFirstTextNodeChild()
     */
	public void testgetFirstTextNodeChild(){
		Element root = factory.getRoot();
		assertNotNull("root not found",root);
		
		assertEquals("content not same",factory.getFirstTextNodeChild(factory.getFirstElementChildByName("content",root)).getNodeValue(),"none" );
		assertEquals("title not same",factory.getFirstTextNodeChild(factory.getFirstElementChildByName("title",root)).getNodeValue(),"Test title" );
		assertEquals("date not same",factory.getFirstTextNodeChild(factory.getFirstElementChildByName("date",root)).getNodeValue(),"Mon Oct 31 12:35:42 CEST 2005" );
		assertEquals("author not same",factory.getFirstTextNodeChild(factory.getFirstElementChildByName("author",root)).getNodeValue(),"FIXME" );
		assertEquals("not has a textNodeChild",factory.getFirstTextNodeChild(root),null );
	}
	
	/**
     *   Test the function getElementChilds()
     */
	public void testgetElementChilds(){
		Element root = factory.getRoot();
		assertNotNull("root not found",root);
		
		assertEquals(factory.getElementChilds(root).length,4);
		assertEquals("title element not title",factory.getElementChilds(root)[0].getNodeName(),"title");
		assertEquals("date element not date",factory.getElementChilds(root)[1].getNodeName(),"date");
		assertEquals("author element not author",factory.getElementChilds(root)[2].getNodeName(),"author");
		assertEquals("content element not content",factory.getElementChilds(root)[3].getNodeName(),"content");
	}
    
    public void testFormatDate() {
        GregorianCalendar calendar = new GregorianCalendar(2005, 9, 31, 10, 21, 35);
        String date = XMLFactory.formatDate(calendar.getTime());
        assertEquals("Date must match format", date, "2005-10-31T10:21:35");
    }

}
