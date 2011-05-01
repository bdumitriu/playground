package test;

/**
 * Created by IntelliJ IDEA.
 * User: Tudor Marian
 * Date: Mar 1, 2003
 * Time: 8:24:24 PM
 * To change this template use Options | File Templates.
 */

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.net.URI;

public class StyleXMLParser
{
	private DocumentBuilder domParser;
	private Document document;

	public StyleXMLParser(String resource)
	{

		try
		{
			domParser = DocumentBuilderFactory.
		        newInstance().newDocumentBuilder();

			InputStream reader = new BufferedInputStream(
			                new FileInputStream(resource)
			        );

			document = domParser.parse(reader);
		}
		catch (ParserConfigurationException e)
		{
			System.out.println("parse config exception");
		}
		catch (FactoryConfigurationError factoryConfigurationError)
		{
			System.out.println("factory config exception");
		}
		catch (IOException e)
		{
			System.out.println("I/O exception");
		}
		catch (SAXException e)
		{
			System.out.println("SAX exception");
		}
	}

	public Map extractColors()
	{
		String defaultStyle;
		Element root = document.getDocumentElement();

		if (!root.hasAttribute("default"))
		{
			switch2HardCodedAttributes();
			return null;
		}
		defaultStyle = root.getAttribute("default");

		// the colors
		Map colorsMap = new HashMap();

		NodeList colorsList = root.getElementsByTagName("colors");
		Node currentColor = null;
		for (int i = 0; i < colorsList.getLength(); i++)
		{
			currentColor = colorsList.item(i);
		}

		NodeList colorList = null;
		if (!currentColor.hasAttributes())
			colorList = currentColor.getChildNodes();

		for (int i = 0; i < colorList.getLength(); i++)
		{
			currentColor = colorList.item(i);

			System.out.println(currentColor.getNodeName() + " " +
			           currentColor.getNodeType());
			if (currentColor.getNodeType() == 3)
			{
				// we have a text node? what da hell is that
				System.out.println("just plain value? " + "<<" +
				        currentColor.getNodeValue() + ">>");
			}

			//System.out.println("for color " + i + " " + currentColor);

		}

		// the styles
		NodeList stylesList = root.getElementsByTagName("style");
		Node currentStyle;
		Node defaultStyleNode = null;

		// lock onto the default style
		for (int i = 0; i < stylesList.getLength(); i++)
		{
			currentStyle = stylesList.item(i);
			String styleID = currentStyle.getAttributes().
			        getNamedItem("name").getNodeValue();

			if (styleID.equals(defaultStyle))
				defaultStyleNode = currentStyle;
		}

		// use that style and manage it

		return colorsMap;
	}

	private void switch2HardCodedAttributes()
	{
	}

	public static String createResourcePath(String base, String filename)
	{
		String separator = System.getProperty("file.separator");
		String currentPath = System.getProperty("user.dir");
		StringBuffer sb = new StringBuffer(currentPath);
		sb.append(separator);
		sb.append(base);
		sb.append(separator);
		sb.append(filename);

		return sb.toString();
	}

	public static void main(String[] args)
	{
		String resource = StyleXMLParser.createResourcePath(
		        "xml", "style.xml"
		);

		StyleXMLParser xp = new StyleXMLParser(resource);


		xp.extractColors();
	}
}
