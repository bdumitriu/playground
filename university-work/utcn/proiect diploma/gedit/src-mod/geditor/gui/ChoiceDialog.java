package geditor.gui;

import geditor.elements.GRootGroup;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;
import java.awt.geom.AffineTransform;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Apr 14, 2004
 * Time: 7:17:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class ChoiceDialog extends JDialog
        implements ActionListener
{

	class GraphicPanel extends JPanel
	{
		private double maxx;
		private double maxy;
		private GRootGroup img;

		public GraphicPanel(double maxx, double maxy, GRootGroup img)
		{
			this.maxx = maxx;
			this.maxy = maxy;
			this.img = img;
		}

		public void paintComponent(Graphics g)
		{
            super.paintComponent(g);
			g.setColor(Color.white);
			g.fillRect(0, 0, 1024, 1024);

			Graphics2D g2 = (Graphics2D) g;
		    AffineTransform trans = g2.getTransform();
			AffineTransform oldtrans = (AffineTransform) trans.clone();

			double sc;
			if ( getHeight() / maxy <  getWidth() / maxx) sc = getHeight() / maxy;
			else sc = getWidth() / maxx;
			trans.scale(sc, sc);
             g2.setTransform(trans) ;
			img.draw((Graphics2D)g);
			g2.setTransform(oldtrans);
		}

	}

	private JPanel panel1;
	private JPanel panel2;
	private GRootGroup img1;
	private GRootGroup img2;
	private static final String SELECT1 = "Select1";
	private static final String SELECT2 = "Select2";

	private double maxx;
	private double maxy;

	private int choice;

	public int getChoice()
	{
		return choice;
	}

	public void setChoice(int choice)
	{
		this.choice = choice;
	}


	public void actionPerformed(ActionEvent e)
	{
		if (SELECT1.equals(e.getActionCommand()))
		{
			choice = 1;
			dispose();
		}
		if (SELECT2.equals(e.getActionCommand()))
		{
			choice = 2;
			dispose();
		}
	}

	public ChoiceDialog(Frame owner, GRootGroup _img1, GRootGroup _img2)
	{
		super(owner, true);
		this.img1 = _img1;
		this.img2 = _img2;

		if (img1.getMaxPoint().x > img2.getMaxPoint().x) maxx =  img1.getMaxPoint().x;
		else maxx = img2.getMaxPoint().x;
		maxx = maxx + maxx / 10;


		if (img1.getMaxPoint().y > img2.getMaxPoint().y) maxy =  img1.getMaxPoint().y;
		else maxy = img2.getMaxPoint().y;
		maxy = maxy + maxy / 10;


		JPanel choicePanel = new JPanel();
		choicePanel.setLayout(new GridLayout(1, 2));
		panel1 = new GraphicPanel(maxx, maxy, img1);
		panel2 = new GraphicPanel(maxx, maxy, img2);

		panel1.setBorder(BorderFactory.createTitledBorder("Local Version"));
		panel2.setBorder(BorderFactory.createTitledBorder("Remote Version"));
		choicePanel.add(panel1);
		choicePanel.add(panel2);
		choicePanel.setPreferredSize(new Dimension(640, 320));


		JPanel buttonPanel = new JPanel();
		JButton button1 = new JButton("Local Version");
		JButton button2 = new JButton("Remote Version");
		button1.setActionCommand(SELECT1);
		button2.setActionCommand(SELECT2);
		button1.addActionListener(this);
		button2.addActionListener(this);
		buttonPanel.add(button1);
		buttonPanel.add(button2);


		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(choicePanel, BorderLayout.CENTER);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		setLocation((int) owner.getLocation().getX() + 100, (int) owner.getLocation().getY() + 100);
		pack();
	}

}
