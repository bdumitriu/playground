package gw.render;


import javax.xml.transform.TransformerException;

import junit.framework.TestCase;


public class URIResolverImplTest extends TestCase {

    /* FIXME: enable these tests

	public void testFileProtocol() {
		MockStorage storage = new MockStorage();
		storage.setProperty("/file1", "content-type", GWML_MIME_TYPE);
		URIResolverImpl resolverImpl = new URIResolverImpl(storage);
		storage.setExpectedRequestedFile("/file1");
		Source source = resolverImpl.resolve("file:///file1", "");
	}

	public void testUnsupportedProtocol() {
		MockStorage storage = new MockStorage();
		storage.setProperty("/file1", "content-type", GWML_MIME_TYPE);
		URIResolverImpl resolverImpl = new URIResolverImpl(storage);
		try {
			Source source = resolverImpl.resolve("unsupported:///file1", "");
			fail();
		} catch (RuntimeException e) { }
	}
	
	public void testUnsupportedExtension() {
		MockStorage storage = new MockStorage();
		storage.setProperty("/file1", "content-type", "unsupported");
		URIResolverImpl resolverImpl = new URIResolverImpl(storage);
		try {
			Source source = resolverImpl.resolve("file:///file1", "");
			fail();
		} catch (Exception e) {	}
	} */
	
	public void testVariable() throws TransformerException {
		/*URIResolverImpl resolverImpl = new URIResolverImpl(new MockStorage(), new GwContext(null));
		URIResolverImpl.registerVariableHandler("test", new ExampleVariableHandler());
		Source source = resolverImpl.resolve("variable:///test?data0&data1", "");
		Element element = (Element) ((JDOMSource) source).getNodes().get(0);
		assertEquals("test", element.getName());
		assertEquals("data0", ((Element) element.getChildren().get(0)).getName());
		assertEquals("data1", ((Element) element.getChildren().get(1)).getName());*/
	}
}
