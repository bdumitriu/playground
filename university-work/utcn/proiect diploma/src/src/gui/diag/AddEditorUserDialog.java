package gui.diag;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * Dialog for adding a new {@link data.EditorUser EditorUser}.
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
		Box colorBox = Box.createHorizontalBox();
		Box buttonBox = Box.createHorizontalBox();

		Box dialogBox = Box.createVerticalBox();

		userTF = new JTextField(15);
		ipTF = new JTextField(15);
		colorCh = new JColorChooser(Color.black);

		okButton = new JButton("Ok");
		okButton.addActionListener(this);

		nameBox.add(new JLabel("User name: "));
		nameBox.add(userTF);

		ipBox.add(new JLabel("IP: "));
		ipBox.add(ipTF);

		colorBox.add(new JLabel("Associated color: "));
		colorBox.add(colorCh);

		buttonBox.add(okButton);

		dialogBox.add(nameBox);
		dialogBox.add(ipBox);
		dialogBox.add(colorBox);
		dialogBox.add(buttonBox);

		getContentPane().add(dialogBox);

		setLocation((int) owner.getLocation().getX() + 100, (int) owner.getLocation().getY() + 100);
		pack();
	}

	/**
	 * Initializes the values of the ip, username and color fields with the given values before displaying them.
	 */
	public AddEditorUserDialog(Frame owner, String ip, String username, Color color)
	{
		this(owner);

		ipTF.setText(ip);
		userTF.setText(username);
		colorCh.setColor(color);
	}

	public String getUsername()
	{
		return userTF.getText();
	}

	public String getIp()
	{
		return ipTF.getText();
	}

	public Color getColor()
	{
		return colorCh.getColor();
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
	private JColorChooser colorCh;

	private JButton okButton;
}
