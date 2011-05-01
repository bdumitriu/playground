package geditor.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * Dialog for adding a new {@link geditor.users.EditorUser EditorUser}.
 * <br /><br />
 * Date: Mar 8, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class AddEditorUserDialog extends JDialog implements ActionListener
{
	public AddEditorUserDialog(Frame owner)
	{
		super(owner, true);

		Box nameBox = Box.createHorizontalBox();
		Box ipBox = Box.createHorizontalBox();
		Box buttonBox = Box.createHorizontalBox();

		Box dialogBox = Box.createVerticalBox();

		userTF = new JTextField(15);
		ipTF = new JTextField(15);

		okButton = new JButton("Ok");
		okButton.addActionListener(this);

		nameBox.add(new JLabel("User name: "));
		nameBox.add(userTF);

		ipBox.add(new JLabel("IP: "));
		ipBox.add(ipTF);

		buttonBox.add(okButton);

		dialogBox.add(nameBox);
		dialogBox.add(ipBox);
		dialogBox.add(buttonBox);

		getContentPane().add(dialogBox);

		setLocation((int) owner.getLocation().getX() + 100, (int) owner.getLocation().getY() + 100);
		pack();
	}

	/**
	 * Initializes the values of the ip and username fields with the given values before displaying them.
	 */
	public AddEditorUserDialog(Frame owner, String ip, String username)
	{
		this(owner);

		ipTF.setText(ip);
		userTF.setText(username);
	}

	public String getUsername()
	{
		return userTF.getText();
	}

	public String getIp()
	{
		return ipTF.getText();
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getSource() == okButton)
		{
			dispose();
		}
	}

	private JTextField userTF;
	private JTextField ipTF;

	private JButton okButton;
}
