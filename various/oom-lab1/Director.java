package lab1;

import java.util.Stack;

import javax.swing.JFrame;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class Director extends DefaultHandler {

	private static final String INPUT_FILE = "src/lab1/spec.xml";

	private FormBuilder builder;
	private Stack<Object> parents;

	private Director(FormBuilder builder) {
		this.builder = builder;
		this.parents = new Stack<Object>();
	}

	public void build(String fileName) {
		SAXParserFactory xmlParserFactory = SAXParserFactory.newInstance();
		try {
			SAXParser xmlParser = xmlParserFactory.newSAXParser();
			xmlParser.parse(fileName, this);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void startDocument() throws SAXException {
		parents.push(builder.startBuilding());
	}

	@Override
	public void endDocument() throws SAXException {
		builder.endBuilding();
		parents.pop();
		assert parents.empty() : "parents stack should have been empty!";
	}

	@Override
	public void startElement(String uri, String localName, String name,
			Attributes attributes) throws SAXException {
		String label = attributes.getValue("label");
		if (name.equalsIgnoreCase("group")) {
			parents.push(builder.startGroup(parents.peek(), label));
		} else if (name.equalsIgnoreCase("radio")) {
			parents.push(builder.startRadioGroup(parents.peek()));
		} else if (name.equalsIgnoreCase("checkbox")) {
			builder.buildCheckbox(parents.peek(), label);
		} else if (name.equalsIgnoreCase("text")) {
			builder.buildText(parents.peek(), label);
		} else if (name.equalsIgnoreCase("button")) {
			builder.buildRadioButton(parents.peek(), label);
		}
	}

	@Override
	public void endElement(String uri, String localName, String name)
			throws SAXException {
		if (name.equalsIgnoreCase("group")) {
			parents.pop();
			builder.endGroup(parents.peek());
		} else if (name.equalsIgnoreCase("radio")) {
			parents.pop();
			builder.endRadioGroup(parents.peek());
		}
	}

	public static void runXmlBuilder() {
		new Director(new XmlFormBuilder()).build(INPUT_FILE);
	}

	public static void runHtmlBuilder() {
		new Director(new HtmlFormBuilder("out.html")).build(INPUT_FILE);
	}

	public static void runGuiBuilder() {
		GuiBuilder guiBuilder = new GuiBuilder();
		new Director(guiBuilder).build(INPUT_FILE);
		JFrame frame =  new JFrame();
		frame.setTitle("GuiBuilder Result");
		frame.getContentPane().add(guiBuilder.getResult());
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}

	public static void main(String[] args) {
		//runXmlBuilder();
		//runHtmlBuilder();
		runGuiBuilder();
	}
}
