using System;
using System.Collections;
using System.Xml;

namespace builder
{
	public class Director
	{
		private FormBuilder builder;
		private Stack parents;

		public Director(FormBuilder builder)
		{
			this.builder = builder;
			this.parents = new Stack();
		}

		public void Build(string fileName) {
			XmlReader reader = XmlReader.Create(fileName);
			while (reader.Read()) {
				if (reader.NodeType == XmlNodeType.Element) {
					StartElement(reader.Name, reader.GetAttribute("label"));
				} else if (reader.NodeType == XmlNodeType.EndElement) {
					EndElement(reader.Name);
				}
			}
			reader.Close();
		}

		public void StartElement(string name, string label) {
			if (name.Equals("form")) {
				parents.Push(builder.StartBuilding());
			} else if (name.Equals("group")) {
				parents.Push(builder.StartGroup(parents.Peek(), label));
			} else if (name.Equals("checkbox")) {
				builder.BuildCheckbox(parents.Peek(), label);
			} else if (name.Equals("text")) {
				builder.BuildText(parents.Peek(), label);
			} else if (name.Equals("radio")) {
				parents.Push(builder.StartRadioGroup(parents.Peek()));
			} else if (name.Equals("button")) {
				builder.BuildRadioButton(parents.Peek(), label);
			}
		}

		public void EndElement(string name) {
			if (name.Equals("form")) {
				builder.EndBuilding();
				parents.Pop();
				if (parents.Count != 0) {
					Console.WriteLine("parents is not empty");
				}
			} else if (name.Equals("group")) {
				parents.Pop();
				builder.EndGroup(parents.Peek());
			} else if (name.Equals("radio")) {
				parents.Pop();
				builder.EndRadioGroup(parents.Peek());
			}
		}
	}
}
