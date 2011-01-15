package lab1;

import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class GuiBuilder implements FormBuilder {

	private class Pair {
		public JPanel panel;
		public ButtonGroup buttonGroup;

		public Pair(JPanel panel, ButtonGroup buttonGroup) {
			this.panel = panel;
			this.buttonGroup = buttonGroup;
		}
	}

	private JPanel result;

	public Object startBuilding() {
		result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
		return result;
	}

	public void endBuilding() {
		// nothing to do here
	}

	public Object startGroup(Object parent, String label) {
		JPanel panel = new JPanel();
		panel.setAlignmentX(Component.LEFT_ALIGNMENT);
		panel.setBorder(BorderFactory.createTitledBorder(label));
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		((JPanel) parent).add(panel);
		return panel;
	}

	public void endGroup(Object parent) {
		// nothing to do here
	}

	public Object startRadioGroup(Object parent) {
		return new Pair((JPanel) parent, new ButtonGroup());
	}

	public void endRadioGroup(Object parent) {
		// nothing to do here
	}

	public void buildCheckbox(Object parent, String label) {
		JCheckBox checkBox = new JCheckBox(label);
		checkBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		((JPanel) parent).add(checkBox);
	}

	public void buildRadioButton(Object parent, String label) {
		Pair parentPair = (Pair) parent;
		JRadioButton radioButton = new JRadioButton(label);
		radioButton.setAlignmentX(Component.LEFT_ALIGNMENT);
		parentPair.buttonGroup.add(radioButton);
		parentPair.panel.add(radioButton);
	}

	public void buildText(Object parent, String label) {
		JPanel textWithLabel = new JPanel(new BorderLayout());
		textWithLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		textWithLabel.add(new JLabel(label + ": "), BorderLayout.WEST);
		textWithLabel.add(new JTextField(), BorderLayout.CENTER);
		((JPanel) parent).add(textWithLabel);
	}

	public JPanel getResult() {
		return result;
	}
}
