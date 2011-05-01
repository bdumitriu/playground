package gui.diag;

import data.RepConfigInfo;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import utils.XMLParsers;

/**
 * Dialog for reading the RMI server and port from the user.
 * <br /><br />
 * Date: Mar 2, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class RMIDialog extends JDialog implements ActionListener
{
	public RMIDialog(Frame owner)
	{
		super(owner, true);

		RepConfigInfo rcInfo = XMLParsers.parseRepConfig();
		defaultRMIServer = rcInfo.getDefaultRmiServer();

		JPanel mainPanel = new JPanel();

		mainPanel.setLayout(new BorderLayout());

		JPanel serverPanel = new JPanel();
		JPanel portPanel = new JPanel();
		JPanel buttonPanel = new JPanel();

		serverTF = new JTextField(defaultRMIServer);
		portTF = new JTextField(defaultRMIPort);

		JButton startButton = new JButton("Start gui");

		startButton.addActionListener(this);

		serverPanel.add(new JLabel("Name or IP of the RMI server: "));
		serverPanel.add(serverTF);

		portPanel.add(new JLabel("Port of the RMI server: "));
		portPanel.add(portTF);

		buttonPanel.add(startButton);

		mainPanel.add(serverPanel, BorderLayout.NORTH);
		mainPanel.add(portPanel, BorderLayout.CENTER);
		mainPanel.add(buttonPanel, BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setLocation((int) owner.getLocation().getX() + 100, (int) owner.getLocation().getY() + 100);
	}

	public int getRmiPort()
	{
		return rmiPort;
	}

	public String getRmiServer()
	{
		return rmiServer;
	}

	public void actionPerformed(ActionEvent e)
	{
		try
		{
			rmiPort = new Integer(portTF.getText()).intValue();
			if ((rmiPort < 1) || (rmiPort > 65535))
			{
				JOptionPane.showMessageDialog(this, "Port should be between 1 and 65535.", "Error",
					JOptionPane.ERROR_MESSAGE);
			}
			else
			{
				rmiServer = serverTF.getText();
				dispose();
			}
		}
		catch (NumberFormatException e1)
		{
			JOptionPane.showMessageDialog(this, "Port should be a number.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
	}

	private int rmiPort;
	private String rmiServer;

	private JTextField serverTF;
	private JTextField portTF;

	private String defaultRMIServer;

	private final String defaultRMIPort = "1099";
}