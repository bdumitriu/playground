package test;

import javax.swing.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * Run the program.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 *
 * Date: Feb 26, 2003
 */
public class Main
{
	public static void main(String[] args)
	{
		JSpiderDocument doc = new JSpiderDocument();
		JSpiderTextPane jtp = new JSpiderTextPane(doc);
		//JTextPane jtp = new JTextPane();

		JFrame frame = new JFrame("jSpider - Java SICStus Prolog Integrated Development EnviRonment");

		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				System.exit(0);
			}
		});

		frame.setSize(400, 400);
		JScrollPane sp = new JScrollPane(jtp);
		sp.getViewport().setBackground(jtp.getBackground());
		frame.getContentPane().add(sp);
		frame.show();
	}
}