package test;

import jade.JadeAgent;
import af.*;

import javax.swing.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.*;
import java.io.File;

/**
 * Created by IntelliJ IDEA.
 *
 * Date: May 27, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class TestAgent2 extends JadeAgent
{
	public ObserverBehaviour obBeh;

	public boolean initialize()
	{
		TestEvent2 event = new TestEvent2(this);

		obBeh = new ObserverBehaviour(1000);
		obBeh.registerObserverEvent(event, 3);
		addAFBehaviour(obBeh);

		return true;
	}

	public void stopMe()
	{
		removeAFBehaviour(obBeh);
	}
}

class TestEvent2 implements ObserverEvent
{
	JFrame frame;
	JLabel label;
	JTextField tf;
	TestAgent2 ta;

	public TestEvent2(final TestAgent2 ta)
	{
		this.ta = ta;

		tf = new JTextField(20);
		label = new JLabel("File existence not checked yet.");

		JPanel panelUp = new JPanel();
		JPanel panelDown = new JPanel();

		panelUp.add(new JLabel("File to observe: "));
		panelUp.add(tf);

		panelDown.add(label);

		frame = new JFrame("Test Agent 2 interface");

		frame.getContentPane().setLayout(new BorderLayout());
		frame.getContentPane().add(panelUp, BorderLayout.CENTER);
		frame.getContentPane().add(panelDown, BorderLayout.SOUTH);

		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				frame.hide();
				ta.stopMe();
			}
		});

		frame.pack();
		frame.show();
	}

	public boolean hasOccured()
	{
		label.setText("File does not exist.");
		File file = new File(tf.getText());
		return file.exists();
	}

	public void callBackWhenOccured()
	{
		label.setText("File exists.");
	}
}