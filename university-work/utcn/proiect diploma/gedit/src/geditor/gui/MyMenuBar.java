package geditor.gui;

import geditor.repository.Repository;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.rmi.RemoteException;
import java.rmi.NotBoundException;
import java.rmi.Naming;
import java.net.MalformedURLException;

public class MyMenuBar extends JMenuBar implements ActionListener
{
	public MyMenuBar()
	{
		super();

		JMenu fileMenu = new JMenu("File");
		JMenu opMenu = new JMenu("Operations");

		JMenuItem exitItem = new JMenuItem("Exit");
		JMenuItem updateItem = new JMenuItem("Update");
		JMenuItem commitItem = new JMenuItem("Commit");
		JMenuItem covItem = new JMenuItem("Check out version");
		JMenuItem fvbdItem = new JMenuItem("Find version by date");

		exitItem.setMnemonic(KeyEvent.VK_X);
		updateItem.setMnemonic(KeyEvent.VK_U);
		commitItem.setMnemonic(KeyEvent.VK_C);
		covItem.setMnemonic(KeyEvent.VK_O);
		fvbdItem.setMnemonic(KeyEvent.VK_F);

		exitItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.ALT_MASK));
		updateItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_U, InputEvent.CTRL_MASK));
		commitItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK));

		exitItem.setActionCommand(cmdExit);
		updateItem.setActionCommand(cmdUpdate);
		commitItem.setActionCommand(cmdCommit);
		covItem.setActionCommand(cmdCheckOut);
		fvbdItem.setActionCommand(cmdFindVersion);

		exitItem.addActionListener(this);
		updateItem.addActionListener(this);
		commitItem.addActionListener(this);
		covItem.addActionListener(this);
		fvbdItem.addActionListener(this);

		fileMenu.addSeparator();
		fileMenu.add(exitItem);

		opMenu.add(updateItem);
		opMenu.add(commitItem);
		opMenu.addSeparator();
		opMenu.add(covItem);
		opMenu.add(fvbdItem);

		add(fileMenu);
		add(opMenu);
	}

	public void actionPerformed(ActionEvent e)
	{
		String cmd = e.getActionCommand();

		if (cmd.equals(cmdExit))
		{
			System.exit(0);
		}
		else if (cmd.equals(cmdUpdate))
		{
			MainFrame.getInstance().getWorkArea().getDocument().update();
		}
		else if (cmd.equals(cmdCommit))
		{
			MainFrame.getInstance().getWorkArea().getDocument().commit();
		}
		else if (cmd.equals(cmdCheckOut))
		{
			String rmiServer = MainFrame.getInstance().getWorkArea().getDocument().getRmiServer();
			int rmiPort = MainFrame.getInstance().getWorkArea().getDocument().getRmiPort();

			String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

			try
			{
				Repository remoteRef = (Repository) Naming.lookup(url);
				int repVersion = remoteRef.getCurrentVersion();

				CheckOutVersionDialog covDialog = new CheckOutVersionDialog(MainFrame.getInstance(),
					repVersion);

				covDialog.show();

				MainFrame.getInstance().getWorkArea().getDocument().checkOutVersion(
					covDialog.getVersion());
			}
			catch (NotBoundException e1)
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.",
					"Error", JOptionPane.ERROR_MESSAGE);
			}
			catch (MalformedURLException e1)
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.",
					"Error", JOptionPane.ERROR_MESSAGE);
			}
			catch (RemoteException e1)
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "RMI error in comunication. " +
					"Check server and port.", "Error", JOptionPane.ERROR_MESSAGE);
			}
		}
		else if (cmd.equals(cmdFindVersion))
		{
			MainFrame.getInstance().getWorkArea().getDocument().findVersionByDate();
		}
	}

	private final static String cmdExit = "exit";
	private final static String cmdUpdate = "update";
	private final static String cmdCommit = "commit";
	private final static String cmdCheckOut = "checkOut";
	private final static String cmdFindVersion = "findVersion";
}
