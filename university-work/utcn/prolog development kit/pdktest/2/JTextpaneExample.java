import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

class JTextpaneExample extends JTextPane
{
	private DefaultStyledDocument doc;
	private Style consoleStyle;
	private Style inputStyle;
	private Style errorStyle;

	public JTextpaneExample()
	{
		super();
		StyleContext sc = new StyleContext();
		doc = new DefaultStyledDocument(sc);

		consoleStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(consoleStyle,
		        new Color(0, 128, 0));
		StyleConstants.setFontFamily(consoleStyle,
			"Monospaced");
		inputStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(inputStyle,
		        new Color(0, 0, 128));
		StyleConstants.setFontFamily(inputStyle,
			"Monospaced");
		errorStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(errorStyle,
		        Color.RED);
		StyleConstants.setFontFamily(errorStyle,
			"Monospaced");

		setDocument(doc);
		setText("SICStus 3.9.1 (x86-win32-nt-4): Wed Jun 19 13:03:11  2002\n" +
			"Licensed to utcluj.ro\n| ?- append([1], [2], R).\n! Existence error in user:append/3\n" +
			"! procedure user:append/3 does not exist\n! goal:  user:append([1], [2], _85)\n| ?- ");
		doc.setCharacterAttributes(0, 84, consoleStyle, true);
		doc.setCharacterAttributes(84, 104, inputStyle, true);
		doc.setCharacterAttributes(104, 217, errorStyle, true);
		doc.setCharacterAttributes(217, 230, consoleStyle, true);
	}

	public static void main(String args[])
	{
		JFrame frame = new JFrame("test");
		
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent event)
			{
				System.exit(0);
			}
		});
		
		frame.setSize(450, 300);
		frame.getContentPane().add(new JTextpaneExample());
		frame.setVisible(true);
	}
}