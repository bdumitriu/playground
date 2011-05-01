import java.applet.Applet;
import java.awt.Graphics;

public class AppletMoo extends Applet
{
	StringBuffer buffer;

	public void init()
	{
		buffer = new StringBuffer();
		addItem("initializing...");
	}

	public void start()
	{
		addItem("starting...");
	}

	public void stop()
	{
		addItem("stopping...");
	}
	
	public void destroy()
	{
		addItem("preparing for unloading...");
	}
	
	void addItem(String newWord)
	{
		System.out.println(newWord);
		buffer.append(newWord);
		repaint();
	}

	public void paint(Graphics g)
	{
		g.drawRect(0, 0, getSize().width - 1, getSize().height - 1);
		g.drawString(buffer.toString(), 5, 15);
	}
}