package lab1;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.PrintWriter;

public class HtmlFormBuilder implements FormBuilder {

	private PrintWriter writer;
	private int n = 0;

	public HtmlFormBuilder(String outputFileName) {
		try {
			writer = new PrintWriter(
					new BufferedOutputStream(
					new FileOutputStream(outputFileName)));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public Object startBuilding() {
		writer.write("<html>\n<body>\n<form>\n");
		return null;
	}

	public void endBuilding() {
		writer.write("</form>\n</body>\n</html>\n");
		writer.close();
	}

	public Object startGroup(Object parent, String label) {
		writer.write(label);
		writer.write("\n<div style=\"border: solid 1px; float: left\">\n");
		return null;
	}

	public void endGroup(Object parent) {
		writer.write("</div>\n<div style=\"clear: left\" />\n");
	}

	public Object startRadioGroup(Object parent) {
		return "rg" + n++;
	}

	public void endRadioGroup(Object parent) {
		// nothing to do here
	}

	public void buildCheckbox(Object parent, String label) {
		writer.write("<input type=\"checkbox\" />" + label + "<br />\n");
	}

	public void buildRadioButton(Object radioGroup, String label) {
		writer.write("<input type=\"radio\" name=\"" + radioGroup + "\">" + label + "<br />\n");
	}

	public void buildText(Object parent, String label) {
		writer.write(label + ": <input type=\"text\" /><br />\n");
	}
}
