import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * This class provides several static methods that allow its user to obtain
 * information from several xml files in an easy to process manner.
 * <br /><br />
 *
 * @author Bogdan DUMITRIU, e-mail: <a href="mailto:bdumitriu@bdumitriu.ro">bdumitriu@bdumitriu.ro</a>
 * @version 0.1
 */

/*
 * Creation date: July 7th, 2002
 */

public class XMLParser
{
	private XMLParser()
	{}

	/**
	 * This method takes the name of an .xml file as an argument,
	 * parses it and returns the obtained Document.
	 */
        public static Document parse(String xmlFileName)
	{
		try
		{
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(xmlFileName);

			return doc;
		}
		catch (FactoryConfigurationError error)
		{
			System.out.println("No DocumentBuilderFactory implementation available.");
		}
		catch (ParserConfigurationException e)
		{
			System.out.println("A DocumentBuilder which satisfies the requested configuration" +
				"could not be created.");
		}
		catch (SAXException e)
		{
			System.out.println("A parse error occured.");
		}
		catch (IOException e)
		{
			System.out.println("An IO error occured.");
		}

		return null;
	}

	/**
	 * Adds key/value pair to the map in the following way: if map already
	 * contains key, than value is added to the ArrayList currently bound
	 * to key; if map doesn't contain key yet, than a new key/value pair
	 * is created and the value is added to the empty newly created ArrayList.
	 */
	private static void addWithCheck(Map map, String key, String value)
	{
		ArrayList list;
		if (map.containsKey(key))
		{
			list = (ArrayList) map.get(key);
		}
		else
		{
			list = new ArrayList();
			map.put(key, list);
		}
		list.add(value);
	}

	/*
	 * Updates the map given as parameter with the actions contained in
	 * &lt_nodeName_&gt tag in the document (the result of parsing the
	 * xml security configuration file - see createSecurityMap for details
	 * on the required xml file structure). Therefore nodeName should
	 * be one of the following: teacher-actions, student-actions, user-actions,
	 * partial-authentication-actions, no-authentication-actions. ArrayListValue
	 * represents the String value to be added in the ArrayList associated
	 * with every action.
	 */
	private static void updateSecurityMap(Map map, Document doc, String nodeName,
		String arrayListValue)
	{
		NodeList nodeList = doc.getElementsByTagName(nodeName);
		for (int i = 0; i < nodeList.getLength(); i++)
		{
                        NodeList actionTags = nodeList.item(i).getChildNodes();
			for (int j = 0; j < actionTags.getLength(); j++)
			{
				NodeList textElements = actionTags.item(j).getChildNodes();
                                for (int k = 0; k < textElements.getLength(); k++)
				{
					Node n = textElements.item(k);
					addWithCheck(map, n.getNodeValue(), arrayListValue);
				}
			}
		}
	}

	/**
	 * This method takes the name of a xml file as an argument and
	 * creates a Map that reflects the file's contents. The file
	 * is supposed to have the format of the Virtual University
	 * security file, i.e.
	 * <pre>
	 * 	&ltsecurity&gt
	 *		&ltteacher-actions&gt
	 *			&ltaction&gtaction_1_ID&lt/action&gt
	 *			&ltaction&gtaction_2_ID&lt/action&gt
	 *			...
	 *			&ltaction&gtaction_n_ID&lt/action&gt
	 *		&lt/teacher-actions&gt
	 *		&ltstudent-actions&gt
	 *			&ltaction&gtaction_1_ID&lt/action&gt
	 *			&ltaction&gtaction_2_ID&lt/action&gt
	 *			...
	 *			&ltaction&gtaction_n_ID&lt/action&gt
	 *		&lt/student-actions&gt
	 *		&ltuser-actions&gt
	 *			&ltaction&gtaction_1_ID&lt/action&gt
	 *			&ltaction&gtaction_2_ID&lt/action&gt
	 *			...
	 *			&ltaction&gtaction_n_ID&lt/action&gt
	 *		&lt/user-actions&gt
	 *		&ltpartial-authentication-actions&gt
	 *			&ltaction&gtaction_1_ID&lt/action&gt
	 *			&ltaction&gtaction_2_ID&lt/action&gt
	 *			...
	 *			&ltaction&gtaction_n_ID&lt/action&gt
	 *		&lt/partial-authentication-actions&gt
	 *		&ltno-authentication-actions&gt
	 *			&ltaction&gtaction_1_ID&lt/action&gt
	 *			&ltaction&gtaction_2_ID&lt/action&gt
	 *			...
	 *			&ltaction&gtaction_n_ID&lt/action&gt
	 *		&lt/no-authentication-actions&gt
	 *	&lt/security&gt
	 * </pre>
	 * The Map it returns contains key/value pairs of the type
	 * java.lang.String/java.util.ArrayList. The String represents the
	 * action id as found in the &ltaction&gt tag and the ArrayList
	 * is a collection of Strings (normally just one). These Strings
	 * can be "teacher", "student", "user", "partial" and "no" as
	 * the &ltaction&gt tag containing the action id is found in the
	 * &ltteacher-actions&gt, &ltstudent-actions&gt, &ltuser-actions&gt,
	 * &ltpartial-authentication-actions&gt or &ltno-authentication-actions&gt
	 * tags respectively. The ArrayList will contain more than a value
	 * if and only if an action id is found in &ltaction&gt tags
	 * belonging to *different* higher level tags, i.e. if two identical
	 * action ids are found under two different &ltaction&gt tags both
	 * belonging to, say, &ltteacher-actions&gt, only one "teacher"
	 * String will be put in the Map.
	 */
	public static Map createSecurityMap(String xmlFileName)
	{
		Map map = new HashMap();
		Document doc = XMLParser.parse(xmlFileName);

		updateSecurityMap(map, doc, "teacher-actions", "teacher");
		updateSecurityMap(map, doc, "student-actions", "student");
		updateSecurityMap(map, doc, "user-actions", "user");
		updateSecurityMap(map, doc, "partial-authentication-actions", "partial");
		updateSecurityMap(map, doc, "no-authentication-actions", "no");

		return map;
	}

	public static void main(String args[])
	{
		Map map = XMLParser.createSecurityMap(args[0]);
		Iterator it = map.entrySet().iterator();
		while (it.hasNext())
		{
			Map.Entry entry = (Map.Entry) it.next();
			System.out.println(entry.getKey() + "/" + entry.getValue());
		}
	}
}
