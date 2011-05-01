import java.awt.*;
import java.applet.Applet;

public class Appletx extends Applet
{
	TextArea ta = new TextArea();
	Button b1 = new Button("button 1");
	Button b2 = new Button("button 2");
	StringBuffer buf = new StringBuffer();

	public Appletx()
	{
		System.out.println("constructor call.");
	}
	
	public void init()
	{
		setLayout(new BorderLayout());
		add("West", b1);
		add("North", b2);
		add("Center", ta);
		print("Method init()");
	}

	public void start()
	{
		print("Method start()");
	}

	public void stop()
	{
		print("Method stop()");
	}

	public void destroy()
	{
		print("Method destroy()");
	}

	public void print(String s)
	{
		System.out.println(s);
		buf.append(s + "\n");
		ta.setText(buf.toString());
		repaint();
	}
}