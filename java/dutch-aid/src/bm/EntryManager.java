package bm;

import org.w3c.dom.Node;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import java.util.TreeMap;
import java.util.Set;
import java.util.ArrayList;
import java.io.File;
import java.io.IOException;

/**
 * Manages the entries in the dictionary.
 * <br /><br />
 * Date: Aug 11, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class EntryManager
{
	public EntryManager()
	{
		try
		{
			domDoc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
			domDoc.appendChild(domDoc.createElement("entries"));
			domDoc.getFirstChild().appendChild(domDoc.createTextNode("\n\t"));
		}
		catch (ParserConfigurationException e)
		{
			e.printStackTrace();
		}

		entryMap = new TreeMap<String, Integer>();
		dataFile = null;
	}

	public EntryManager(File xmlFile)
	{
		setDataFile(xmlFile);
		load();
	}

	/**
	 * Sets the current data file to <code>xmlFile</code>.
	 *
	 * @param xmlFile the new data file to use
	 */
	public void setDataFile(File xmlFile)
	{
		dataFile = xmlFile;
	}

	/**
	 * Looks for the dutch word in the list of entries and returns the index of the entry or -1 if no entry for
	 * the supplied word exists.
	 *
	 * @param dutchWord the dutch word defining the entry
	 * @return the index of the entry or -1 if no entry exists for <code>dutchWord</code>
	 */
	private int getEntryIndex(String dutchWord)
	{
		Object temp = entryMap.get(dutchWord);

		if (temp == null)
		{
			return -1;
		}
		else
		{
			Integer index = (Integer) temp;
			return index.intValue();
		}
	}

	/**
	 * Returns the entry corresponding to the dutch word <code>dutchWord</code>. If no entry exists, null is
	 * returned.
	 *
	 * @return the entry corresponding to the dutch word <code>dutchWord</code>
	 */
	public Entry getEntry(String dutchWord)
	{
		int index = getEntryIndex(dutchWord);
		if (index == -1)
		{
			return null;
		}

		Node entry = domDoc.getFirstChild().getChildNodes().item(index);

		return nodeToEntry(entry);
	}

	/**
	 * Returns the number of available entries.
	 *
	 * @return the number of available entries.
	 */
	public int getSize()
	{
		return entryMap.size();
	}

	/**
	 * Returns an ArrayList<Entry> containing all the available entries.
	 *
	 * @return an ArrayList<Entry> containing all the available entries.
	 */
	public ArrayList<Entry> getEntries()
	{
		Set<String> keys = entryMap.keySet();

		ArrayList<Entry> entries = new ArrayList<Entry>(getSize());
		for (String key : keys)
		{
			entries.add(getEntry(key));
		}

		return entries;
	}

	/**
	 * Loads the entries from the current <code>dataFile</code> into memory.
	 */
	public void load()
	{
		if (dataFile == null)
		{
			return;
		}

		entryMap = new TreeMap<String, Integer>();

		try
		{
			domDoc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(dataFile);

			// this returns the element for the "entries" tag
			Node root = domDoc.getFirstChild();

			if (domDoc.getFirstChild().getLastChild() != null)
			{
				domDoc.getFirstChild().getLastChild().setNodeValue("\n\t");
			}

			// this returns the list of elements of type "entry" interspersed with TextNode's
			NodeList entries = root.getChildNodes();

			for (int i = 0; i < entries.getLength(); i++)
			{
				Node curEntry = entries.item(i);

				// igonore TextNode's (they only contain whitespace)
				if ((curEntry.getNodeType() == Node.ELEMENT_NODE) &&
					(curEntry.getNodeName() == "entry"))
				{
					NodeList entryData = curEntry.getChildNodes();
					String dutchWord = "";
					for (int j = 0; j < entryData.getLength(); j++)
					{
						Node curNode = entryData.item(j);
						if (curNode.getNodeType() == Node.ELEMENT_NODE &&
							curNode.getNodeName().equals("dutch-word"))
						{
							if (curNode.getFirstChild() != null)
							{
								dutchWord = curNode.getFirstChild().getNodeValue();
							}
						}
					}

					entryMap.put(dutchWord, new Integer(i));
				}
			}
		}
		catch (SAXException e)
		{
			e.printStackTrace();
		}
		catch (IOException e)
		{
			//e.printStackTrace();
			try
			{
				domDoc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
				domDoc.appendChild(domDoc.createElement("entries"));
				domDoc.getFirstChild().appendChild(domDoc.createTextNode("\n\t"));
			}
			catch (ParserConfigurationException ex)
			{
				e.printStackTrace();
			}
		}
		catch (ParserConfigurationException e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * Saves the entries from memory to the current <code>dataFile</code> (which is set by means of the
	 * {@link #setDataFile(java.io.File) setDataFile} method).
	 */
	public void save()
	{
		if (dataFile == null)
		{
			return;
		}

		try
		{
			Node lastChild = domDoc.getFirstChild().getLastChild();

			boolean changed = false;
			if (lastChild != null && lastChild.getNodeType() == Document.TEXT_NODE &&
				lastChild.getNodeValue().equals("\n\t"))
			{
				lastChild.setNodeValue("\n");
				changed = true;
			}

			DOMSource source = new DOMSource(domDoc);
			StreamResult result = new StreamResult(dataFile);

			TransformerFactory.newInstance().newTransformer().transform(source, result);

			if (changed)
			{
				lastChild.setNodeValue("\n\t");
			}
		}
		catch (TransformerException e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * Adds a new element to the internal in-memory structures.
	 *
	 * @param dutchWord Dutch word
	 * @param romanianWord Romanian word
	 * @param englishWord English word
	 * @param type type of word (noun, verb, adjective, etc.)
	 * @param article the article of the noun (het, de) or "" if type != "noun"
	 * @param frequency the frequency of the word
	 * @param extra the Dutch extra of the word
	 * @param sampleExpression a sampleExpression expression/sentence using the word
	 */
	public void addElement(String dutchWord, String romanianWord, String englishWord, String type, String article,
		String frequency, String extra, String sampleExpression)
	{
		Element root = domDoc.createElement("entry");

		Element temp;

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("dutch-word");
		temp.appendChild(domDoc.createTextNode(dutchWord));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("romanian-word");
		temp.appendChild(domDoc.createTextNode(romanianWord));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("english-word");
		temp.appendChild(domDoc.createTextNode(englishWord));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("type");
		temp.appendChild(domDoc.createTextNode(type));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("article");
		temp.appendChild(domDoc.createTextNode(article));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("frequency");
		temp.appendChild(domDoc.createTextNode(frequency));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("extra-info");
		temp.appendChild(domDoc.createTextNode(extra));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t\t"));
		temp = domDoc.createElement("sample-expression");
		temp.appendChild(domDoc.createTextNode(sampleExpression));
		root.appendChild(temp);

		root.appendChild(domDoc.createTextNode("\n\t"));

		// the position of the element node that will be added (used in entryMap)
		int position = domDoc.getFirstChild().getChildNodes().getLength();

		domDoc.getFirstChild().appendChild(root);
		domDoc.getFirstChild().appendChild(domDoc.createTextNode("\n\t"));

		entryMap.put(dutchWord, new Integer(position));
	}

	/**
	 * Adds a new element to the internal in-memory structures.
	 *
	 * @param entry the entry containing all the necessary data
	 */
	public void addElement(Entry entry)
	{
		addElement(entry.getDutchWord(), entry.getRomanianWord(), entry.getEnglishWord(), entry.getType(),
			entry.getArticle(), entry.getFrequency(), entry.getExtra(), entry.getSampleExpression());
	}

	/**
	 * Modifies the entry identified by <code>currentDutchWord</code> so that it contains the new specified values.
	 *
	 * @param currentDutchWord the dutch word identifying the entry to be modified
	 * @param dutchWord Dutch word
	 * @param romanianWord Romanian word
	 * @param englishWord English word
	 * @param type type of word (noun, verb, adjective, etc.)
	 * @param article the article of the noun (het, de) or "" if type != "noun"
	 * @param frequency the frequency of the word
	 * @param extra the Dutch extra of the word
	 * @param sampleExpression a sampleExpression expression/sentence using the word
	 * @return true if an entry corresponding to <code>currentDutchWord</code> was found (and updated), false
	 *	otherwise
	 */
	public boolean modifyElement(String currentDutchWord, String dutchWord, String romanianWord, String englishWord,
		String type, String article, String frequency, String extra, String sampleExpression)
	{
		Entry entry = new Entry(dutchWord, romanianWord, englishWord, type, article, frequency, extra,
			sampleExpression);
		return modifyElement(currentDutchWord, entry);
	}

	/**
	 * Modifies the entry identified by <code>currentDutchWord</code> so that it contains the values from
	 * <code>entry<code>.
	 *
	 * @param currentDutchWord the dutch word identifying the entry to be modified
	 * @param entry the entry containing all the necessary data
	 * @return true if an entry corresponding to <code>currentDutchWord</code> was found (and updated), false
	 *	otherwise
	 */
	public boolean modifyElement(String currentDutchWord, Entry entry)
	{
		int index = getEntryIndex(currentDutchWord);
		if (index == -1)
		{
			return false;
		}

		Node corrNode = domDoc.getFirstChild().getChildNodes().item(index);

		changeNodeValues(corrNode, entry);

		// update the entryMap if necessary
		if (!currentDutchWord.equals(entry.getDutchWord()))
		{
			entryMap.put(entry.getDutchWord(), entryMap.remove(currentDutchWord));
		}

		return true;
	}

	/**
	 * Provided <code>node</code> is the root node containing an Entry, an Entry object is created, filled with
	 * values from the node and returned.
	 */
	private Entry nodeToEntry(Node node)
	{
		Entry entry = new Entry();
		NodeList values = node.getChildNodes();
		int n = values.getLength();

		for (int i = 0; i < n; i++)
		{
			Node curNode = values.item(i);
			if (curNode.getNodeType() == Node.TEXT_NODE)
			{
				continue;
			}

			String name = curNode.getNodeName();
			String value = "";
			if (curNode.getFirstChild() != null)
			{
				value = curNode.getFirstChild().getNodeValue();
			}
			if (name.equals("dutch-word"))
			{
				entry.setDutchWord(value);
			}
			else if (name.equals("romanian-word"))
			{
				entry.setRomanianWord(value);
			}
			else if (name.equals("english-word"))
			{
				entry.setEnglishWord(value);
			}
			else if (name.equals("type"))
			{
				entry.setType(value);
			}
			else if (name.equals("article"))
			{
				entry.setArticle(value);
			}
			else if (name.equals("frequency"))
			{
				entry.setFrequency(value);
			}
			else if (name.equals("extra-info"))
			{
				entry.setExtra(value);
			}
			else if (name.equals("sample-expression"))
			{
				entry.setSampleExpression(value);
			}
		}

		return entry;
	}

	/**
	 * Provided <code>node</code> is the root node containing an Entry, all the values of its children are modified
	 * so that they contain the values found in the <code>entry</code> object.
	 */
	private void changeNodeValues(Node node, Entry entry)
	{
		NodeList values = node.getChildNodes();
		int n = values.getLength();

		for (int i = 0; i < n; i++)
		{
			Node curNode = values.item(i);
			if (curNode.getNodeType() == Node.TEXT_NODE)
			{
				continue;
			}

			String name = curNode.getNodeName();
			String value = "";
			if (name.equals("dutch-word"))
			{
				value = entry.getDutchWord();
			}
			else if (name.equals("romanian-word"))
			{
				value = entry.getRomanianWord();
			}
			else if (name.equals("english-word"))
			{
				value = entry.getEnglishWord();
			}
			else if (name.equals("type"))
			{
				value = entry.getType();
			}
			else if (name.equals("article"))
			{
				value = entry.getArticle();
			}
			else if (name.equals("frequency"))
			{
				value = entry.getFrequency();
			}
			else if (name.equals("extra-info"))
			{
				value = entry.getExtra();
			}
			else if (name.equals("sample-expression"))
			{
				value = entry.getSampleExpression();
			}

			if (curNode.getFirstChild() != null)
			{
				curNode.getFirstChild().setNodeValue(value);
			}
			else
			{
				curNode.appendChild(domDoc.createTextNode(value));
			}
		}
	}

	/**
	 * Returns the DOM node which represents the data from <code>entry</code>.
	 * @param entry
	 * @return
	 */
	private Node entryToNode(Entry entry)
	{
		Node node = domDoc.createElement("entry");
		Node tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		Node dn = domDoc.createElement("dutch-word");
		dn.appendChild(domDoc.createTextNode(entry.getDutchWord()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		dn = domDoc.createElement("romanian-word");
		dn.appendChild(domDoc.createTextNode(entry.getRomanianWord()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		dn = domDoc.createElement("english-word");
		dn.appendChild(domDoc.createTextNode(entry.getEnglishWord()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		dn = domDoc.createElement("type");
		dn.appendChild(domDoc.createTextNode(entry.getType()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		dn = domDoc.createElement("article");
		dn.appendChild(domDoc.createTextNode(entry.getArticle()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		dn = domDoc.createElement("frequency");
		dn.appendChild(domDoc.createTextNode(entry.getFrequency()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		dn = domDoc.createElement("extra-info");
		dn.appendChild(domDoc.createTextNode(entry.getExtra()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t\t");
		node.appendChild(tn);
		dn = domDoc.createElement("sample-expression");
		dn.appendChild(domDoc.createTextNode(entry.getSampleExpression()));
		node.appendChild(dn);
		tn = domDoc.createTextNode("\n\t");
		node.appendChild(tn);

		return node;
	}

	private File dataFile;
	private Document domDoc;
	private TreeMap<String, Integer> entryMap;
}
