package gui.diag;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * Dialog to request a version number to check out.
 * <br /><br />
 * Date: Mar 9, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class CheckOutVersionDialog extends JDialog implements ActionListener
{
	/**
	 * @param owner the owner of this component
	 * @param maxVersion the maximum version number which is valid
	 */
	public CheckOutVersionDialog(Frame owner, int maxVersion)
	{
		super(owner, true);

		this.maxVersion = maxVersion;
		parentFrame = owner;

		versionTF = new JTextField(5);

		okButton = new JButton("Ok");
		okButton.addActionListener(this);

		JPanel versionPanel = new JPanel();
		JPanel buttonPanel = new JPanel();

		versionPanel.add(new JLabel("Version number to check out: "));
		versionPanel.add(versionTF);

		buttonPanel.add(okButton);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(versionPanel, BorderLayout.CENTER);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		setLocation((int) owner.getLocation().getX() + 100, (int) owner.getLocation().getY() + 100);
		pack();
	}

	public int getVersion()
	{
		return version;
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getSource() == okButton)
		{
			try
			{
				version = (new Integer(versionTF.getText())).intValue();

				if (version < 0 || version > maxVersion)
				{
					JOptionPane.showMessageDialog(parentFrame, "Version must be between 0 and " +
						maxVersion + ".", "Error", JOptionPane.ERROR_MESSAGE);
					version = -1;
				}
				else
				{
					dispose();
				}
			}
			catch (NumberFormatException exc)
			{
				version = -1;
				JOptionPane.showMessageDialog(parentFrame, "Version must be a number.", "Error",
					JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	private int maxVersion;

	private JTextField versionTF;
	private JButton okButton;

	private Frame parentFrame;

	private int version;
}
