import javax.swing.*;
import java.awt.*;
import java.awt.event.*; 

public class Model
{
	public static void main(String args[])
	{
		JFrame frame = new JFrame();
		JSlider slider = new JSlider(0, 100, 50);
		frame.getContentPane().setLayout(new FlowLayout());
		frame.getContentPane().add(new Label("move the slider..."));
		frame.getContentPane().add(slider);
		frame.getContentPane().add(new JProgressBar(slider.getModel()));
		
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				System.exit(0);
			}
		});

		frame.pack();
		frame.show();
	}
}
