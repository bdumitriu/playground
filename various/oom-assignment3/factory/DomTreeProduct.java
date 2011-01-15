package ass3.factory;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Concrete Product using DOM.
 *
 * @author bdumitriu
 */
public class DomTreeProduct implements TreeProduct {

	private Node currentNode;

	private DomTreeProduct[] children;

	public DomTreeProduct(String file) {
		try {
			DocumentBuilder builder =
					DocumentBuilderFactory.newInstance().newDocumentBuilder();
			Document doc = builder.parse(new File(file));

			// I'm assuming here there is only a single top-level node in the
			// XML document; if this is not the case, only the first top-level
			// node will be used in order to create directories, there rest will
			// be ignored
			this.currentNode = doc.getFirstChild();
			prepareChildren();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private DomTreeProduct(Node node) {
		this.currentNode = node;
		prepareChildren();
	}

	@Override
	public TreeProduct[] getChildren() {
		return children;
	}

	private void prepareChildren() {
		List<DomTreeProduct> children = new LinkedList<DomTreeProduct>();
		NodeList childNodes = this.currentNode.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); i++) {
			Node childNode = childNodes.item(i);
			if (childNode.getNodeType() == Node.ELEMENT_NODE) {
				children.add(new DomTreeProduct(childNode));
			}
		}
		this.children = children.toArray(new DomTreeProduct[0]);
	}

	@Override
	public String getName() {
		return this.currentNode.getNodeName();
	}

	@Override
	public boolean isLeaf() {
		return this.children.length == 0;
	}
}
