package client;

import javax.swing.*;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class IMClient
{
	public static void main(String[] args)
	{
		if (args.length != 2)
		{
			System.out.println("Usage: java IMClient <host> <port>\n");
			System.exit(0);
		}

		IMCore core = new IMCore(args[0], Integer.parseInt(args[1]));

		IMGUI clientGUI = new IMGUI(core);
		clientGUI.setBounds(0, 0, 800, 600);
		clientGUI.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		clientGUI.setResizable(false);
		clientGUI.setVisible(true);
	}
}
