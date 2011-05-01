package geditor.gui;

import geditor.engine.tree.GraphicDocument;
import geditor.engine.tree.UpdateType;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * A simple component that allows the user to choose between several types of conflict resolution methods.
 * <br /><br />
 * Date: Apr 16, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class InteractionChoice extends JPanel implements ActionListener
{
	public InteractionChoice()
	{
		// create the radio buttons
		automaticRB = new JRadioButton("automatic");
		automaticRB.setActionCommand(cmdAutomatic);
		automaticRB.addActionListener(this);

		manualRB = new JRadioButton("manual");
		manualRB.setActionCommand(cmdManual);
		manualRB.addActionListener(this);

		automaticRB.setSelected(true);

		ButtonGroup choiceGroup = new ButtonGroup();
		choiceGroup.add(automaticRB);
		choiceGroup.add(manualRB);

		JPanel choiceGroupPanel = new JPanel(new GridLayout(0, 1));

		choiceGroupPanel.add(new JLabel("Conflict"));
		choiceGroupPanel.add(new JLabel("resolution"));
		choiceGroupPanel.add(new JLabel("method:"));
		choiceGroupPanel.add(automaticRB);
		choiceGroupPanel.add(manualRB);

		add(choiceGroupPanel);
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getActionCommand().equals(cmdAutomatic))
		{
			MainFrame.getInstance().getWorkArea().getDocument().setUpdateType(UpdateType.localWinner);
			MainFrame.getInstance().getUserListGui().setSynchButtonStyle(true);
		}
		else if (e.getActionCommand().equals(cmdManual))
		{
			MainFrame.getInstance().getWorkArea().getDocument().setUpdateType(UpdateType.choice);
			MainFrame.getInstance().getUserListGui().setSynchButtonStyle(false);
		}
	}

	private JRadioButton automaticRB;
	private JRadioButton manualRB;

	private final String cmdAutomatic = "Automatic";
	private final String cmdManual = "Manual";
}
