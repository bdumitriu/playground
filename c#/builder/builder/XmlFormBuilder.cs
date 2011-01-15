using System;

namespace builder
{
	public class XmlFormBuilder : FormBuilder
	{
		public Object StartBuilding() {
			Console.WriteLine("<form>");
			return 1;
		}

		public void EndBuilding() {
			Console.WriteLine("</form>");
		}

		public Object StartGroup(Object parent, string label) {
			int level = Indent(parent);
			Console.WriteLine("<group label=\"{0}\">", label);
			return level + 1;
		}

		public void EndGroup(Object parent) {
			Indent(parent);
			Console.WriteLine("</group>");
		}

		public void BuildCheckbox(Object parent, string label) {
			Indent(parent);
			Console.WriteLine("<checkbox label=\"{0}\" />", label);
		}

		public Object StartRadioGroup(Object parent) {
			int level = Indent(parent);
			Console.WriteLine("<radio>");
			return level + 1;
		}

		public void BuildRadioButton(Object radioGroup, string label) {
			Indent(radioGroup);
			Console.WriteLine("<button label=\"{0}\" />", label);
		}

		public void EndRadioGroup(Object parent) {
			Indent(parent);
			Console.WriteLine("</radio>");
		}

		public void BuildText(Object parent, string label) {
			Indent(parent);
			Console.WriteLine("<text label=\"{0}\" />", label);
		}

		private int Indent(Object levelObj) {
			int level = (int) levelObj;
			for (int i = 0; i < level; i++) {
				Console.Write("\t");
			}
			return level;
		}
	}
}
