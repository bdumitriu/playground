import javax.swing.*;
import javax.swing.text.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Feb 20, 2003
 * Time: 7:35:18 PM
 * To change this template use Options | File Templates.
 */
public class Main
{
	public static void main(String[] args)
	{
		JSpiderDocument doc = new JSpiderDocument();
		JSpiderTextPane jtp = new JSpiderTextPane(doc);
		DefaultHighlighter h = (DefaultHighlighter) jtp.getHighlighter();
		doc.setHighlighter(h);

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
