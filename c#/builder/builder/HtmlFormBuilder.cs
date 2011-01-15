using System;
using System.IO;

namespace builder
{
	public class HtmlFormBuilder : FormBuilder
	{
		private TextWriter writer;
		private int n = 0;

		public HtmlFormBuilder(string outputFileName)
		{
			writer = new StreamWriter(outputFileName);
		}

		public Object StartBuilding() {
			writer.WriteLine("<html>\n<body>\n<form>");
			return null;
		}

		public void EndBuilding() {
			writer.WriteLine("</form>\n</body>\n</html>");
			writer.Close();
		}

		public Object StartGroup(Object parent, string label) {
			writer.WriteLine(label);
			writer.WriteLine("<div style=\"border: solid 1px; float: left\">");
			return null;
		}

		public void EndGroup(Object parent) {
			writer.WriteLine("</div>\n<div style=\"clear: left\" />");
		}

		public void BuildCheckbox(Object parent, string label) {
			writer.WriteLine("<input type=\"checkbox\" />{0}<br />", label);
		}

		public Object StartRadioGroup(Object parent) {
			return "rg" + n++;
		}

		public void BuildRadioButton(Object radioGroup, string label) {
			
			writer.WriteLine("<input type=\"radio\" name=\"{0}\">{1}<br />", radioGroup, label);
		}

		public void EndRadioGroup(Object parent) {
			// nothing to do here
		}

		public void BuildText(Object parent, string label) {
			writer.WriteLine("{0}: <input type=\"text\" /><br />", label);
		}
	}
}
