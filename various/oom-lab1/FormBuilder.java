package lab1;

public interface FormBuilder {
	public Object startBuilding();
	public void endBuilding();
	public Object startGroup(Object parent, String label);
	public void endGroup(Object parent);
	public void buildCheckbox(Object parent, String label);
	public Object startRadioGroup(Object parent);
	public void buildRadioButton(Object radioGroup, String label);
	public void endRadioGroup(Object parent);
	public void buildText(Object parent, String label);
}
