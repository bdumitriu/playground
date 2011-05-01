package gui.diag;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * Dialog to present the user with two options from which (s)he should choose one.
 * <br /><br />
 * Date: Mar 5, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class ChoiceDialog extends JDialog implements ActionListener
{
	public ChoiceDialog(Frame owner)
	{
		super(owner, true);

		setTitle("conflict resolution");

		pane1 = new JTextPane();
		pane2 = new JTextPane();

		JScrollPane jsp1 = new JScrollPane(pane1);
		jsp1.setPreferredSize(new Dimension(100, 50));

		JScrollPane jsp2 = new JScrollPane(pane2);
		jsp2.setPreferredSize(new Dimension(100, 50));

		pane1.setEditable(false);
		pane2.setEditable(false);

		JPanel choicePanel = new JPanel();
		JPanel buttonPanel = new JPanel();

		choicePanel.setLayout(new GridLayout(1, 2));

		choicePanel.add(jsp1);
		choicePanel.add(jsp2);

		button1 = new JButton("Select 1");
		button2 = new JButton("Select 2");
		button3 = new JButton("Expand context");

		button1.addActionListener(this);
		button2.addActionListener(this);
		button3.addActionListener(this);

		buttonPanel.add(button1);
		buttonPanel.add(button2);
		buttonPanel.add(button3);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(choicePanel, BorderLayout.CENTER);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		setLocation((int) owner.getLocation().getX() + 100, (int) owner.getLocation().getY() + 100);
		pack();
	}

	public void setChoice(int choiceNo, String choiceText)
	{
		if (choiceNo == 1)
		{
			choice1 = choiceText;
			pane1.setText(choice1);
		}
		else if (choiceNo == 2)
		{
			choice2 = choiceText;
			pane2.setText(choice2);
		}
		pack();
	}

	public int getChoice()
	{
		return choice;
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getSource() == button1)
		{
			choice = 1;
			dispose();
		}
		else if (e.getSource() == button2)
		{
			choice = 2;
			dispose();
		}
		else if (e.getSource() == button3)
		{
			choice = 0;
			dispose();
		}
	}

	private JTextPane pane1;
	private JTextPane pane2;

	private JButton button1;
	private JButton button2;
	private JButton button3;

	private String choice1;
	private String choice2;

	private int choice;
}
