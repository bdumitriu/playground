package chatAdmin.server;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.net.MalformedURLException;
import java.rmi.server.*;
import java.rmi.RemoteException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.registry.*;

public class ServerManager
{
	private static boolean extraSafeFlag = true;
	private static JFrame frame = new JFrame("Chat Admin Server Manager");
	private static JLabel label = new JLabel();
	private static JLabel info = new JLabel("Info:");
	private static JButton start = new JButton("Start server");
	private static JButton stop = new JButton("Stop server");
	private static ChatAdminServer server;
	private static Registry reg;
		
	public static void main(String args[])
	{
		try
		{
			 reg = LocateRegistry.createRegistry(2500);
		}
		catch (RemoteException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		
		stop.setEnabled(false);
		frame.getContentPane().setLayout(new BorderLayout());
		frame.getContentPane().add("Center", label);
		frame.getContentPane().add("North", start);
		frame.getContentPane().add("South", stop);
		frame.getContentPane().add("West", info);
		label.setHorizontalAlignment(SwingConstants.CENTER);
		label.setVerticalAlignment(SwingConstants.CENTER);
		label.setText("The server is currently stopped.");
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent l)
			{
				System.exit(0);
			}
		});
		frame.setSize(400, 100);
		frame.setResizable(false);
		
		start.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				if (extraSafeFlag == true)
				{
					start.setEnabled(false);
					stop.setEnabled(true);
					
					try
					{
						server = new ChatAdminServer();
						Naming.rebind("//192.168.0.1" +
							":2500/chatAdminServ",
							server);
					}
					catch (RemoteException ex)
					{
						System.out.println(ex.
							getMessage());
						ex.printStackTrace();
					}
					catch (MalformedURLException ex)
					{
						System.out.println(ex.
							getMessage());
						ex.printStackTrace();
					}
					
					label.setText("Server has been" +
						" started.");
					extraSafeFlag = false;
				}
			}
        	});
        	stop.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				if (extraSafeFlag == false)
				{
					start.setEnabled(true);
					stop.setEnabled(false);
					
					try
					{
						Naming.unbind("//192.168.0.1" +
							":2500/chatAdminServ");
						server.stopServer();
						UnicastRemoteObject.
							unexportObject(
							server, true);
						server = null;
						
					}
					catch (MalformedURLException ex)
					{
						System.out.println(ex.
							getMessage());
						ex.printStackTrace();
					}
					catch (NotBoundException ex)
					{
						System.out.println(ex.
							getMessage());
						ex.printStackTrace();
					}
					catch (RemoteException ex)
					{
						System.out.println(ex.
							getMessage());
						ex.printStackTrace();
					}
					
					label.setText("Server has been" +
						" stopped.");
					extraSafeFlag = true;
				}
			}
        	});
		
		frame.setVisible(true);
	}
}
