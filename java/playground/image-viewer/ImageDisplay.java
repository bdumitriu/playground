/*
 * Author: Bogdan DUMITRIU
 * Date:   28.02.2002
 */

import java.io.*;
import javax.swing.*;
import java.awt.*;

class ImageDisplay extends JPanel
{
	public static void main(String args[])
	{
		if (args.length != 1)
		{
			System.out.println("\tUsage: java ImageDisplay filename.ext");
			System.exit(0);
		}
		String filename = args[0];
		long startTime = System.currentTimeMillis();
		ImageIcon icon = new ImageIcon(filename);
		JLabel label = new JLabel(icon);
		JFrame f = new JFrame(filename);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setSize(icon.getIconWidth(), icon.getIconHeight());
		f.getContentPane().add(label);
		f.setVisible(true);
		long stopTime = System.currentTimeMillis();
		System.out.println("Load time: " + (stopTime - startTime));
	}
}
