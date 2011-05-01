package test;

import javax.swing.*;
import javax.swing.text.StyledDocument;
import javax.swing.text.View;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * Extension of JTextPane which turns off wrapping.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 *
 * Date: Feb 26, 2003
 */
public class JSpiderTextPane extends JTextPane implements
        ComponentListener, KeyListener, MouseListener
{

	private int noOfEvents;
	private StyledDocument doc;
	private View view;

	private int componentSizeX;
	private int componentSizeY;

	public JSpiderTextPane(StyledDocument doc)
	{
		super(doc);
		this.doc = doc;
		noOfEvents = 0;

		componentSizeX = getBounds().width;
		componentSizeY = getBounds().height;

		view = createDefaultEditorKit().getViewFactory().
		        create(doc.getDefaultRootElement());

		this.addComponentListener(this);
		this.addKeyListener(this);
		this.addMouseListener(this);
	}

	public boolean getScrollableTracksViewportWidth()
	{
		//return true;
		return false;
	}

	// @author Tudor Marian
	public void componentResized(ComponentEvent e)
	{
		int newWidth = getBounds().width;
		int newHeight = getBounds().height;

		if (componentSizeX < newWidth ||
		        componentSizeY < newHeight)
		{
			//printOffsets();
		}

		componentSizeX = newWidth;
		componentSizeY = newHeight;

		System.out.println("RESIZED COMPONENT");
	}

	public void componentMoved(ComponentEvent e)
	{
	}

	public void componentShown(ComponentEvent e)
	{
	}

	public void componentHidden(ComponentEvent e)
	{
	}

	public void keyTyped(KeyEvent e)
	{
		//printOffsets();
		Point p1 = this.getVisibleRect().getLocation();

		int x2 = getVisibleRect().x + getVisibleRect().width;
		int y2 = getVisibleRect().y + getVisibleRect().height;

		Point p2 = new Point(x2, y2);

		int startOffset = viewToModel(p1);
		int endOffset = viewToModel(p2);

		System.out.println("syntax highlighting form : " +
		        startOffset + " to " + endOffset);
	}

	public void keyPressed(KeyEvent e)
	{
	}

	public void keyReleased(KeyEvent e)
	{
	}

	private Dimension offsetBounds()
	{
		return null;
	}

	private void printOffsets()
	{
		Dimension offsets = offsetBounds();

		System.out.println("<highlighting from : " +
		        offsets.width + " until : " +
		        offsets.height + ">");
	}

	public void mouseClicked(MouseEvent e)
	{
		Point p1 = e.getPoint();
		int charOffset = viewToModel(p1);
		System.out.println("clicked at about : " + charOffset);
	}

	public void mousePressed(MouseEvent e)
	{
	}

	public void mouseReleased(MouseEvent e)
	{
	}

	public void mouseEntered(MouseEvent e)
	{
	}

	public void mouseExited(MouseEvent e)
	{
	}

}
