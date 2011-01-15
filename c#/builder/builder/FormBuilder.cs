using System;

namespace builder
{
	public interface FormBuilder
	{
		Object StartBuilding();
		void EndBuilding();
		Object StartGroup(Object parent, string label);
		void EndGroup(Object parent);
		void BuildCheckbox(Object parent, string label);
		Object StartRadioGroup(Object parent);
		void BuildRadioButton(Object radioGroup, string label);
		void EndRadioGroup(Object parent);
		void BuildText(Object parent, string label);
	}
}
