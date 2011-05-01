package geditor.gui;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 4, 2003
 * Time: 5:19:23 PM
 */
public class ColorBar extends JComponent
{
	public final static int FG_COLOR = 31;
	public final static int BG_COLOR = 32;

	public static final int ACTIVATED = 10;
	public static final int DEACTIVATED = 11;

	protected JTextField textField;
	protected Color currentColor;

	/**
	 * the type of the component (FG_COLOR or BG_COLOR)
	 */
	protected int type;

	/**
	 * the state of the component (ACTIVATED or DEACTIVATED)
	 */
	protected int state;

	public ColorBar(int type)
	{
		this.type = type;
		this.state = 0;

		textField = new JTextField();
		textField.setEditable(false);
		textField.setFocusable(false);

		switch (type)
		{
			case FG_COLOR:
				textField.setToolTipText("Change border fgColor");
				textField.setBackground(Color.black);
				currentColor = Color.black;
				break;
			case BG_COLOR:
				textField.setToolTipText("Change background fgColor");
				textField.setBackground(Color.white);
				currentColor = Color.white;
				break;
		}

		textField.addMouseListener(new MouseHandler());

		JPanel panel = new JPanel(new BorderLayout());
		Border compoundBorder = BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(), BorderFactory.createLoweredBevelBorder());
		panel.setBorder(compoundBorder);
		panel.setBounds(1, 1, 28, 28);

		panel.add(textField);

		setPreferredSize(new Dimension(30, 30));

		add(panel);
	}

	/**
	 * Changes the status of this fgColor bar to <code>status</code>. Possible values: ColorBar.ACTIVATED,
	 * ColorBar.DEACTIVATED.
	 */
	public void setStatus(int state)
	{
		if (state == ACTIVATED || state == DEACTIVATED)
		{
			this.state = state;

			if (state == DEACTIVATED)
			{
				textField.setBackground(Color.lightGray);
			}
			else if (state == ACTIVATED)
			{
				textField.setBackground(currentColor);
			}
		}
	}

	/**
	 * Programatically sets the fgColor to newColor.
	 */
	public void setColor(Color newColor)
	{
		currentColor = newColor;
		textField.setBackground(newColor);

		if (type == FG_COLOR)
		{
			MainFrame.getInstance().getWorkArea().setFgColor(newColor);
		}
		if (type == BG_COLOR)
		{
			MainFrame.getInstance().getWorkArea().setBgColor(newColor);
		}
	}

	class MouseHandler extends MouseAdapter
	{
		public void mouseClicked(MouseEvent e)
		{
			if (state == ACTIVATED)
			{
				new ColorChooserFrame(currentColor).setVisible(true);
			}
		}
	}

	class ColorChooserFrame extends JDialog implements ActionListener
	{
		protected JColorChooser cc;
		protected JButton ok, cancel;

		public ColorChooserFrame(Color initColor)
		{
			super(MainFrame.getInstance(), "Color chooser", true);
			setSize(300, 300);

			if (initColor == null)
			{
				initColor = Color.black;
			}

			cc = new JColorChooser(initColor);
			cc.setBorder(BorderFactory.createTitledBorder("Choose object fgColor"));

			ok = new JButton("Ok");
			cancel = new JButton("Cancel");
			ok.addActionListener(this);
			cancel.addActionListener(this);

			JPanel panel = new JPanel();
			panel.add(ok);
			panel.add(cancel);

			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			Dimension frameSize = getSize();
			if (frameSize.height > screenSize.height)
			{
				frameSize.height = screenSize.height;
			}
			if (frameSize.width > screenSize.width)
			{
				frameSize.width = screenSize.width;
			}
			setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);

			getContentPane().setLayout(new BorderLayout());
			getContentPane().add(cc, BorderLayout.CENTER);
			getContentPane().add(panel, BorderLayout.SOUTH);
		}

		public void actionPerformed(ActionEvent e)
		{
			String cmd = e.getActionCommand();
			if (cmd.equals("Ok"))
			{
				if (type == FG_COLOR)
				{
					MainFrame.getInstance().getWorkArea().setFgColor(cc.getColor());
				}
				if (type == BG_COLOR)
				{
					MainFrame.getInstance().getWorkArea().setBgColor(cc.getColor());

				}

				currentColor = cc.getColor();
				textField.setBackground(cc.getColor());

				this.dispose();
			}
			else if (cmd.equals("Cancel"))
			{
				this.dispose();
			}
		}

	}
}