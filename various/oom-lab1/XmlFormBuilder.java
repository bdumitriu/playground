package lab1;

public class XmlFormBuilder implements FormBuilder {

	public Object startBuilding() {
		System.out.println("<form>");
		return 1;
	}

	public void endBuilding() {
		System.out.println("</form>");
	}

	public Object startGroup(Object parent, String label) {
		int indentationLevel = (Integer) parent;
		indent(indentationLevel);
		System.out.println("<group label=\"" + label + "\" />");
		return indentationLevel + 1;
	}

	public void endGroup(Object parent) {
		int indentationLevel = (Integer) parent;
		indent(indentationLevel);
		System.out.println("</group>");
	}

	public Object startRadioGroup(Object parent) {
		Integer indentationLevel = (Integer) parent;
		indent(indentationLevel);
		System.out.println("<radio>");
		return indentationLevel + 1;
	}

	public void endRadioGroup(Object parent) {
		int indentationLevel = (Integer) parent;
		indent(indentationLevel);
		System.out.println("</radio>");
	}

	public void buildCheckbox(Object parent, String label) {
		Integer indentationLevel = (Integer) parent;
		indent(indentationLevel);
		System.out.println("<checkbox label=\"" + label + "\" />");
	}

	public void buildRadioButton(Object parent, String label) {
		Integer indentationLevel = (Integer) parent;
		indent(indentationLevel);
		System.out.println("<radio label=\"" + label + "\" />");
	}

	public void buildText(Object parent, String label) {
		Integer indentationLevel = (Integer) parent;
		indent(indentationLevel);
		System.out.println("<text label=\"" + label + "\" />");
	}

	private void indent(int level) {
		StringBuilder indentation = new StringBuilder();
		for (int i = 0; i < level; i++) {
			indentation.append("\t");
		}
		System.out.print(indentation);
	}
}
