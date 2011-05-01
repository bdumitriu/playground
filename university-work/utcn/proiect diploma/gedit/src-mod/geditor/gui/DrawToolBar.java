package geditor.gui;

import geditor.engine.tree.GraphicDocument;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 24, 2003
 * Time: 2:24:55 PM
 */
public class DrawToolBar extends JToolBar implements ActionListener
{
	// status
	public final static int ACTIVATED = 10;
	public final static int DEACTIVATED = 11;
	// button id
	public final static int BUTTON_ARROW = 21;
	public final static int BUTTON_LINE = 22;
	public final static int BUTTON_ELLIPSE = 23;
	public final static int BUTTON_RECTANGLE = 24;
	public final static int BUTTON_TEXT = 25;

	// drawing operation
	protected JToggleButton ellipse;
	protected JToggleButton rectangle;
	protected JToggleButton text;
	protected JToggleButton arrow;
	protected JToggleButton line;

	protected ColorBar fgColorBar;
	protected ColorBar bgColorBar;

	protected JToggleButton currentButton;

	public DrawToolBar()
	{
		super();
		setFloatable(false);

		//arrow toggle button
		arrow = new JToggleButton(new ImageIcon("resources/images/select.gif"));
		arrow.setActionCommand("arrow");
		arrow.setSelected(true);
		arrow.addActionListener(this);
		arrow.setFocusable(false);
		arrow.setToolTipText("Select Objects");
		add(arrow);

		addSeparator();

		this.currentButton = arrow;

		// line toggle button
		line = new JToggleButton(new ImageIcon("resources/images/line.gif"));
		line.setActionCommand("line");
		line.setSelected(false);
		line.addActionListener(this);
		line.setFocusable(false);
		line.setToolTipText("Draw Line");
		add(line);

		// ellipse toggle button
		ellipse = new JToggleButton(new ImageIcon("resources/images/ellipse.gif"));
		ellipse.setActionCommand("ellipse");
		ellipse.setSelected(false);
		ellipse.addActionListener(this);
		ellipse.setFocusable(false);
		ellipse.setToolTipText("Draw Ellipse");
		add(ellipse);

		// rectangle toggle button
		rectangle = new JToggleButton(new ImageIcon("resources/images/rectangle.gif"));
		rectangle.setActionCommand("rectangle");
		rectangle.setSelected(false);
		rectangle.addActionListener(this);
		rectangle.setFocusable(false);
		rectangle.setToolTipText("Draw Rectangle");
		add(rectangle);

		// text toggle button
		text = new JToggleButton(new ImageIcon("resources/images/text.gif"));
		text.setActionCommand("text");
		text.setSelected(false);
		text.addActionListener(this);
		text.setFocusable(false);
		text.setToolTipText("Draw text");
		add(text);

		addSeparator();

		// foreground color
		//JPanel panel = new JPanel();
		//panel.setMaximumSize(new Dimension(65, 60));
		//panel.setLayout(new GridLayout(1, 2));
		//panel.setFocusable(false);
		fgColorBar = new ColorBar(ColorBar.FG_COLOR);
		fgColorBar.setFocusable(false);
		add(fgColorBar);
		// background color
		bgColorBar = new ColorBar(ColorBar.BG_COLOR);
		bgColorBar.setFocusable(false);
		add(bgColorBar);

		addSeparator();
		//add(panel);
	}

	public void pressDrawingButton(int type)
	{
		currentButton.setSelected(false);
		switch (type)
		{
			case BUTTON_ARROW:
				currentButton = arrow;
				MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_SELECTION);
				break;
			case BUTTON_ELLIPSE:
				currentButton = ellipse;
				MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWELLIPSE);
				break;
			case BUTTON_LINE:
				currentButton = line;
				MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWLINE);
				break;
			case BUTTON_RECTANGLE:
				currentButton = rectangle;
				MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWRECTANGLE);
				break;
			case BUTTON_TEXT:
				currentButton = text;
				MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWTEXT);
				break;
		}
		currentButton.setSelected(true);
	}

	public void setStatus(int status)
	{
		fgColorBar.setStatus(status);
		bgColorBar.setStatus(status);
		boolean s = (status == ACTIVATED);
		ellipse.setEnabled(s);
		rectangle.setEnabled(s);
		text.setEnabled(s);
		arrow.setEnabled(s);
		line.setEnabled(s);
	}

	public void actionPerformed(ActionEvent e)
	{
		String cmd = e.getActionCommand();
		currentButton.setSelected(false);
		//get the new button
		currentButton = (JToggleButton) e.getSource();
		if (cmd.startsWith("arrow"))
			MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_SELECTION);
		else if (cmd.startsWith("line"))
			MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWLINE);
		else if (cmd.startsWith("ellipse"))
			MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWELLIPSE);
		else if (cmd.startsWith("rectangle"))
			MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWRECTANGLE);
		else if (cmd.startsWith("text"))
			MainFrame.getInstance().getWorkArea().setMode(WorkArea.MODE_DRAWTEXT);

		currentButton.setSelected(true);
	}

	/**
	 * Getter methods
	 */

	public ColorBar getFgColorBar()
	{
		return fgColorBar;
	}

	public ColorBar getBckColorBar()
	{
		return bgColorBar;
	}

	public JToggleButton getLine()
	{
		return line;
	}

	public ColorBar getBgColorBar()
	{
		return bgColorBar;
	}

	public JToggleButton getArrow()
	{
		return arrow;
	}

	public JToggleButton getText()
	{
		return text;
	}

	public JToggleButton getRectangle()
	{
		return rectangle;
	}

	public JToggleButton getEllipse()
	{
		return ellipse;
	}
}
