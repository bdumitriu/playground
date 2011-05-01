package utils;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import data.RepConfigInfo;

/**
 * This class defines a handler for a rep-config.xml file.
 * <br /><br />
 * Date: Feb 25, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class RepConfigHandler extends DefaultHandler
{
	/**
	 * Builds a new RepConfigHandler which takes a {@link RepConfigInfo RepConfigInfo} object as a parameter. It
	 * will fill this object with data during parsing.
	 *
	 * @param rcInfo the container object to use for storing the data read from the XML file.
	 */
	public RepConfigHandler(RepConfigInfo rcInfo)
	{
		globalXMLString = new StringBuffer();
		this.rcInfo = rcInfo;
	}

	/**
	 * @see org.xml.sax.ContentHandler#startElement(String, String, String, Attributes)
	 */
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException
	{
		globalXMLString.append(qName);
	}

	/**
	 * @see org.xml.sax.ContentHandler#endElement(String, String, String)
	 */
	public void endElement(String uri, String localName, String qName) throws SAXException
	{
		globalXMLString.delete(globalXMLString.length() - qName.length(), globalXMLString.length());
	}

	/**
	 * @see org.xml.sax.ContentHandler#characters(char[], int, int)
	 */
	public void characters(char ch[], int start, int length) throws SAXException
	{
		String xmlString = globalXMLString.toString();
		if (xmlString.equals(PORT_STRING))
		{
			rcInfo.setRmiPort(charArrayToString(ch, start, length));
		}
		else if (xmlString.equals(PATH_STRING))
		{
			rcInfo.setDirectoryPath(charArrayToString(ch, start, length));
		}
		else if (xmlString.equals(DEFAULT_SERVER_STRING))
		{
			rcInfo.setDefaultRmiServer(charArrayToString(ch, start, length));
		}
	}

	/**
	 * Returns a String containing <code>length</code> characters from the <code>ch</code> array starting from the
	 * <code>start</code>th character. If the <code>ch</code> array doesn't hold enough characters, a shorter
	 * (possibly empty) String is returned.
	 * <br /><br />
	 * @param ch an array of characters
	 * @param start the position in the <code>ch</code> array to start from
	 * @param length the number of characters to read
	 * @return the String built as described above.
	 */
	private String charArrayToString(char ch[], int start, int length)
	{
		StringBuffer sb = new StringBuffer();

		if (start < 0)
		{
			length += start;
			start = 0;
		}

		int startIndex = Math.min(start, ch.length);
		int endIndex = Math.min(start + length, ch.length);

		for (int i = startIndex; i < endIndex; i++)
		{
			sb.append(ch[i]);
		}

		return sb.toString();
	}

	/**
	 * The following identifiers are used in the charaters method see if the current position in the XML document
	 * contains the specific information we are interested in. The idea is that we use the globalXMLString variable
	 * to dynamically maintain the current position in the XML document accross startElement and endElement method
	 * calls by adding/deleting the name of the XML tag to/from this string. Then, in the characters method we
	 * compare this globalXMLString with all of the identifiers below in order to see whether we are inside a tag
	 * holding data we are interested in.
	 */
	private static String PORT_STRING = "repositoryrmiserverport";
	private static String PATH_STRING = "repositorydirectorypath";
	private static String DEFAULT_SERVER_STRING = "repositorydefaultrmiserver";

	/**
	 * The globalXMLString variable described above.
	 */
	private StringBuffer globalXMLString;

	/**
	 * The data container used to store relevant data read from the XML file.
	 */
	private RepConfigInfo rcInfo;
}
