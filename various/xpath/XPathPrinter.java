/*
 * Copyright 2009 QTronic GmbH. All rights reserved.
 */
package xpath;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.StringWriter;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.*;

import org.w3c.dom.Node;
import org.xml.sax.InputSource;

/**
 *
 *
 * @author Bogdan Dumitriu
 */
public class XPathPrinter {

	public final static String XPATH_QUERY = "/project/target";

	public static void main(String[] args) throws XPathExpressionException, FileNotFoundException, TransformerException {
		XPath xpath = XPathFactory.newInstance().newXPath();
		final XPathExpression expression = xpath.compile(XPATH_QUERY);
		final InputSource inputSource = new InputSource(new FileReader("d:/work/software_dev/common/build.xml"));
		final Object result = expression.evaluate(inputSource, XPathConstants.NODE);
		final Node node = (Node) result;

		final Transformer transformer = TransformerFactory.newInstance().newTransformer();
		transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		final StreamResult streamResult = new StreamResult(new StringWriter());
		final DOMSource source = new DOMSource(node);
		transformer.transform(source, streamResult);
		String xmlString = streamResult.getWriter().toString();
		System.out.println(xmlString);
	}
}
