import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;

public class ChatClient
{
	public static void main(String args[])
	{
		JFrame frame = new JFrame("Bogdan's Chat Room");
		JTextArea jta = new JTextArea("Test text.", 10, 80);
		
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				System.exit(0);
			}
		});

		jta.setEditable(false);
		jta.setBorder(new LineBorder(Color.lightGray, 10));
		frame.setSize(400, 400);
		frame.getContentPane().setLayout(new GridLayout(2, 1));
		frame.getContentPane().add(jta);
		frame.pack();
		frame.setVisible(true);
	}
}

