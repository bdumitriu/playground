package geditor.tools;

import org.xml.sax.SAXException;

import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import java.io.IOException;
import java.io.File;

/**
 * This class provides various parsers for all types of xml files used by this application.
 * <br /><br />
 * Date: Feb 25, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class XMLParsers
{
	/**
	 * Parses a rep-config.xml file and creates a {@link RepConfigInfo RepConfigInfo} object containing the
	 * relevant data.
	 * <br /><br />
	 * @return a {@link RepConfigInfo RepConfigInfo} object containing the relevant data.
	 */
	public static RepConfigInfo parseRepConfig()
	{
		// create an empty RepConfigInfo object to pass to the handler
		RepConfigInfo rcInfo = new RepConfigInfo();

		try
		{
			// create a builder factory
			SAXParserFactory factory = SAXParserFactory.newInstance();
			//factory.setValidating(false);

			// create the parser
			SAXParser parser = factory.newSAXParser();

			// parse the file
			parser.parse(new File("etc/rep-config.xml"), new RepConfigHandler(rcInfo));
		}
		catch (SAXException e)
		{
			e.printStackTrace();
			System.exit(1);
		}
		catch (ParserConfigurationException e)
		{
			e.printStackTrace();
			System.exit(1);
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.exit(1);
		}

		return rcInfo;
	}
}
