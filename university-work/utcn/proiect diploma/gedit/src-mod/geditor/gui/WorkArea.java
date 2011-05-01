package geditor.gui;

import geditor.elements.*;
import geditor.engine.tree.*;
import geditor.engine.operations.*;


import javax.swing.*;
import java.awt.*;
import java.awt.geom.Point2D;
import java.awt.print.Printable;
import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.image.BufferedImage;

import java.awt.event.*;
import java.util.ArrayList;


/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 24, 2003
 * Time: 5:29:47 PM
 */
public class WorkArea extends JPanel implements MouseListener, MouseMotionListener, KeyListener, Printable
{
	// drawing modes constants
	public static final int MODE_IDLE = 100;		// no action can be performed
	public static final int MODE_SELECTION = 101;		// only for selecting objects
	public static final int MODE_DRAWLINE = 102;		// only for drawing line
	public static final int MODE_DRAWELLIPSE = 103;		// only for drawing ellipse
	public static final int MODE_DRAWRECTANGLE = 104;	// only for drawing rectangle
	public static final int MODE_DRAWTEXT = 105;		// only for drawing rectangle
	public static final int MODE_RESIZE = 106;		// only for drawing rectangle
	public static final int MODE_MOVE = 1130;

	// submode constants
	public static final int SUB_MODE_MOVE = 107;
	public static final int SUB_MODE_IDLE = 108;
	public static final int SUB_MODE_RESIZE = 109;

	public static final int SUB_MODE_RESIZE_NW = 109;
	public static final int SUB_MODE_RESIZE_NE = 110;
	public static final int SUB_MODE_RESIZE_HORISONTAL = 111;	// only for drawing rectangle
	public static final int SUB_MODE_RESIZE_VERTICAL = 112;		// only for drawing rectangle

	public Point2DDouble refPoint;




	// curent mode
	private int mode;
	private int subMode;

	private boolean multiselection;

	private GShape workShape;

	/** points used in drawing, resizing and moving the shapes */

	private Point2DDouble previousPoint;

	private BufferedImage bufferedImage;

	/** the new stuff */
	private GraphicDocument grDoc;
	private Color fgColor;
	private Color bgColor;

	double resize_tx;
	double resize_ty;

	double move_dx;
	double move_dy;


	boolean dragged;
	boolean selection_changed;
	public WorkArea()
	{
		this.mode = MODE_SELECTION;
		this.subMode = SUB_MODE_IDLE;
		this.multiselection = false;

		this.workShape = null;
		this.previousPoint = null;
		//create the buffered image
		this.bufferedImage = new BufferedImage(1280, 1000, BufferedImage.TYPE_INT_RGB);
		//add listeners
		addMouseListener(this);
		addMouseMotionListener(this);
		addKeyListener(this);

		//set cursor
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		grDoc = new GraphicDocument();
	}

	public void setMode(int mode)
	{
		switch (mode)
		{
			case MODE_SELECTION:
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				break;
			case MODE_IDLE:
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				break;
			case MODE_DRAWLINE:
			case MODE_DRAWTEXT:
			case MODE_DRAWRECTANGLE:
			case MODE_DRAWELLIPSE:
				setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
				break;
		}
		this.mode = mode;
	}

	public int getMode()
	{
		return mode;
	}

	public Color getBgColor()
	{
		return bgColor;
	}

	public void setBgColor(Color bgColor)
	{
		this.bgColor = bgColor;
		System.out.println("bgColor = " + bgColor);
	}

	public Color getFgColor()
	{
		return fgColor;
	}

	public void setFgColor(Color fgColor)
	{
		this.fgColor = fgColor;
		System.out.println("fgColor = " + fgColor);
	}

	public void paint(Graphics g)
	{
		update(g);
	}

	public void update(Graphics g)
	{
		Graphics2D g2 = (Graphics2D) g;
		Graphics2D bufferedGraphics2D = bufferedImage.createGraphics();
		bufferedGraphics2D.fillRect(0, 0, 1280, 1000);
		grDoc.getRoot().draw((Graphics2D) bufferedImage.getGraphics());
		//fileController.getTree().draw((Graphics2D) bufferedImage.getGraphics());
		g2.drawImage(bufferedImage, 0, 0, this);
	}


	public void changeSelection(MouseEvent e)
	{
		grabFocus();

		// select object
		if (mode == MODE_SELECTION)
		{
			// if we are in selection mode
			// application.getMainFrame().getObjectInspector().loadSettings();
			Point2DDouble point = new Point2DDouble(e.getPoint().x, e.getPoint().y);
			grDoc.selectObject(point, multiselection);
			repaint();
		}
	}

	public void mouseClicked(MouseEvent e)
	{

	}

	public GraphicDocument getDocument()
	{
		return grDoc;
	}

	public void mousePressed(MouseEvent e)
	{
		refPoint = new Point2DDouble(e.getPoint().x, e.getPoint().y);
		previousPoint = new Point2DDouble(e.getPoint().x + 1, e.getPoint().y + 1);

		switch (mode)
		{
			case MODE_DRAWLINE:
				GLine line = new GLine(refPoint, previousPoint, 0, fgColor);
				grDoc.addShape(line);
				workShape = line;
				break;
			case MODE_DRAWRECTANGLE:
				GRectangle rectangle = new GRectangle((Point2DDouble) (refPoint.clone()), previousPoint, 0, fgColor, bgColor);
				grDoc.addShape(rectangle);
				workShape = rectangle;
				break;
			case MODE_DRAWELLIPSE:
				GEllipse ellipse = new GEllipse((Point2DDouble) refPoint.clone(), previousPoint, 0, fgColor, bgColor);
				grDoc.addShape(ellipse);
				workShape = ellipse;
				break;
			case MODE_DRAWTEXT:
				GText text = new GText((Point2DDouble) refPoint.clone(), previousPoint, 0, fgColor, bgColor, "Text");
				grDoc.addShape(text);
				workShape = text;
				break;
			case MODE_SELECTION:
				selection_changed = false;
				if (grDoc.getSelection().size() != 1)
				{
					changeSelection(e);
					selection_changed = true;
				}

				if (grDoc.getSelection().size() == 1)
				{
					GShape shape = (GShape)grDoc.getSelection().get(0);

					refPoint = shape.getScalingReferencePoint(e.getPoint().getX(), e.getPoint().getY());
					previousPoint = new Point2DDouble(e.getPoint().x, e.getPoint().y);
					workShape = shape;

					if (refPoint != null)
					{
						mode = MODE_RESIZE;
						resize_tx = 1;
						resize_ty = 1;
						break;
					}

					if (shape.insideObject(new Point2DDouble(e.getPoint().x,  e.getPoint().y)))
					{
							mode = MODE_MOVE;
							move_dx = 0; move_dy = 0;
							dragged = false;
							break;
					}

					changeSelection(e);
					selection_changed = true;

					if (grDoc.getSelection().size() == 1)
					{
						 shape = (GShape)grDoc.getSelection().get(0);
						workShape = shape;
						if (shape.insideObject(new Point2DDouble(e.getPoint().x,  e.getPoint().y)))
						{
								mode = MODE_MOVE;
								move_dx = 0; move_dy = 0;
								dragged = false;
								break;
						}
					}
				}

				break;
		}
	}

	public void mouseReleased(MouseEvent e)
	{
		if (workShape != null)
		{
			Point2DDouble curPoint = new Point2DDouble(e.getPoint().x, e.getPoint().y);

			switch (mode)
			{
				case MODE_DRAWLINE:
				case MODE_DRAWRECTANGLE:
				case MODE_DRAWELLIPSE:
				case MODE_DRAWTEXT:
				{
					if (curPoint.x == refPoint.x)
					{
						curPoint.x++;
					}
					if (curPoint.y == refPoint.y)
					{
						curPoint.y++;
					}

					workShape.scale(refPoint.x, refPoint.y,
						(curPoint.x - refPoint.x) / (previousPoint.x - refPoint.x),
						(curPoint.y - refPoint.y) / (previousPoint.y - refPoint.y));

					grDoc.deselectAll();

					try
					{
						grDoc.select(workShape.getId(), false);
					}
					catch (InvalidIdException e1)
					{}


					MainFrame.getInstance().getDrawToolBar().pressDrawingButton(DrawToolBar.BUTTON_ARROW);

					grDoc.addToHistory(new CreateOperation(workShape.getParent().getId(), workShape));

					workShape = null;

					repaint();
					break;
				}
				case MODE_RESIZE:
				{
					if (curPoint.x == refPoint.x)
					{
						curPoint.x++;
					}
					if (curPoint.y == refPoint.y)
					{
						curPoint.y++;
					}

					workShape.scale(refPoint.x, refPoint.y,
						(curPoint.x - refPoint.x) / (previousPoint.x - refPoint.x),
							(curPoint.y - refPoint.y) / (previousPoint.y - refPoint.y));

					resize_tx *= (curPoint.x - refPoint.x) / (previousPoint.x - refPoint.x);
					resize_ty *= (curPoint.y - refPoint.y) / (previousPoint.y - refPoint.y);

					ScaleOperation scop = new ScaleOperation(workShape.getId(), refPoint.x, refPoint.y,  resize_tx, resize_ty, workShape.getRfx(), workShape.getRfy());
					grDoc.addToHistory(scop);

					workShape = null;

					MainFrame.getInstance().getDrawToolBar().pressDrawingButton(DrawToolBar.BUTTON_ARROW);

					updateObjectInspector();
					repaint();
					break;
				}
				case MODE_MOVE:
				{
					if (!dragged  && !multiselection && !selection_changed)
					{
						System.out.println("HI");

				        mode = MODE_SELECTION;
						changeSelection(e);
						repaint();
						break;
					}
					workShape.moveWith(curPoint.x - previousPoint.x, curPoint.y - previousPoint.y);
					move_dx += curPoint.x - previousPoint.x;
					move_dy += curPoint.y - previousPoint.y;

					grDoc.addToHistory(new TranslationOperation(workShape.getId(), move_dx, move_dy));

					workShape = null;

					MainFrame.getInstance().getDrawToolBar().pressDrawingButton(DrawToolBar.BUTTON_ARROW);

					updateObjectInspector();
					repaint();
					break;
				}
			}

			previousPoint = null;
		}
	}

	public void mouseDragged(MouseEvent e)
	{

		dragged = true;
		if (workShape != null)
		{
			Point2DDouble curPoint = new Point2DDouble(e.getPoint().x, e.getPoint().y);

			switch (mode)
			{
				case MODE_DRAWLINE:
				case MODE_DRAWRECTANGLE:
				case MODE_DRAWELLIPSE:
				case MODE_DRAWTEXT:
				{
					if (Math.round(curPoint.x) == Math.round(refPoint.x))
					{
						curPoint.x++;
					}
					if (Math.round(curPoint.y) == Math.round(refPoint.y))
					{
						curPoint.y++;
					}

					workShape.scale(refPoint.x, refPoint.y,
						(curPoint.x - refPoint.x) / (previousPoint.x - refPoint.x),
						(curPoint.y - refPoint.y) / (previousPoint.y - refPoint.y));

					repaint();
					break;
				}
				case MODE_RESIZE :
				{
					if (Math.round(curPoint.x) == Math.round(refPoint.x))
					{
						curPoint.x++;
					}
					if (Math.round(curPoint.y) == Math.round(refPoint.y))
					{
						curPoint.y++;
					}

					workShape.scale(refPoint.x, refPoint.y,
						(curPoint.x - refPoint.x) / (previousPoint.x - refPoint.x),
						(curPoint.y - refPoint.y) / (previousPoint.y - refPoint.y));

					resize_tx *= (curPoint.x - refPoint.x) / (previousPoint.x - refPoint.x);
					resize_ty *= (curPoint.y - refPoint.y) / (previousPoint.y - refPoint.y);

					updateObjectInspector();
					repaint();
					break;
				}
				case MODE_MOVE:
				{
					workShape.moveWith(curPoint.x - previousPoint.x, curPoint.y - previousPoint.y);
					move_dx += curPoint.x - previousPoint.x;
					move_dy += curPoint.y - previousPoint.y;
					updateObjectInspector();
					repaint();
					break;
				}
			}

			previousPoint = curPoint;
		}
	}

	public void mouseMoved(MouseEvent e)
	{

	}

	public void mouseEntered(MouseEvent e)
	{}

	public void mouseExited(MouseEvent e)
	{}

	public void shiftDown()
	{
		if ((mode == MODE_SELECTION) && !multiselection)
		{
			multiselection = true;
			grDoc.deselectAll();
			repaint();
			System.out.println("Multiselection enable.");
		}
	}

	public void shiftUp()
	{
		if ((mode == MODE_SELECTION) && multiselection)
		{
			multiselection = false;
			System.out.println("Multiselection disable.");
		}
	}

	public void keyTyped(KeyEvent e)
	{}

	public void keyPressed(KeyEvent e)
	{
		switch (e.getKeyCode())
		{
			case KeyEvent.VK_SHIFT:
				shiftDown();
				break;
		}
	}

	public void keyReleased(KeyEvent e)
	{
		switch (e.getKeyCode())
		{
			case KeyEvent.VK_SHIFT:
				shiftUp();
				break;
			case KeyEvent.VK_DELETE:
				doDelete();
				break;
		}
	}

	public int print(Graphics graphics, PageFormat pageFormat, int pageIndex) throws PrinterException
	{
		if (pageIndex >= 1)
		{
			return Printable.NO_SUCH_PAGE;
		}
		Graphics2D g2d = (Graphics2D) graphics;
		g2d.drawImage(bufferedImage, (int) pageFormat.getPaper().getImageableX(), (int) pageFormat.getPaper().getImageableY(), 1280, 1000, this);
		g2d.drawRect((int) pageFormat.getPaper().getImageableX(), (int) pageFormat.getPaper().getImageableY(), (int) pageFormat.getPaper().getImageableWidth(), (int) pageFormat.getPaper().getImageableHeight());
		g2d.scale(((double) 1280 / (pageFormat.getPaper().getWidth() - pageFormat.getPaper().getImageableX())),
			((double) 1000 / pageFormat.getPaper().getHeight() - pageFormat.getPaper().getImageableY()));
		return Printable.PAGE_EXISTS;
	}

	public void doGroup()
	{
		if (grDoc.getSelection().size() > 1)
		{
			Operation op;
			ArrayList children = new ArrayList();

			for (int i = 0; i < grDoc.getSelection().size(); i++)
				children.add(grDoc.getSelection().get(i));

			op = new GroupOperation(grDoc.getRoot().getId(),  children);
			op.applyTo(grDoc.getRoot());

			grDoc.addToHistory(op);

			grDoc.deselectAll();

			repaint();
		}
	}

	public void doUnGroup()
	{
		if (grDoc.getSelection().size() == 1 && grDoc.getSelection().get(0) instanceof GGroup)
		{
                GGroup group = (GGroup) grDoc.getSelection().get(0);

				if (group.getParent() != grDoc.getRoot()) return;  
				ArrayList children = new ArrayList();
				for (int i = 0; i < group.getNrChildren(); i++)
					children.add(group.childAt(i).clone());
				Operation op = new UngroupOperation(group.getParent().getId(), group.getId(), children);
				op.applyTo(grDoc.getRoot());
				grDoc.addToHistory(op);

				grDoc.deselectAll();
				repaint();
		}
	}

	public void doBringToFront()
	{
		if (grDoc.getSelection().size() == 1)
		{
			GShape shape = (GShape) grDoc.getSelection().get(0);

			double z = shape.getParent().getMinZ() - 1;

			Operation op;

			op = new SetZOperation(shape.getId(), z);
			op.applyTo(grDoc.getRoot());
			grDoc.addToHistory(op);

			repaint();

		}
	}

	public void doSendToBack()
	{
		if (grDoc.getSelection().size() == 1)
		{
			GShape shape = (GShape) grDoc.getSelection().get(0);


			double z = shape.getParent().getMaxZ() + 1;


			Operation op;

			op = new SetZOperation(shape.getId(), z);
			op.applyTo(grDoc.getRoot());
			grDoc.addToHistory(op);


			repaint();
		}
	}

	public void doDelete()
	{
		if (mode == MODE_SELECTION)
		{
			ArrayList sel = grDoc.getSelection();
			for (int i = 0; i < sel.size(); i++)
			{
				GShape shape = (GShape) sel.get(i);
				Operation op = new DeleteOperation(shape.getParent().getId(), shape);
				op.applyTo(grDoc.getRoot());
				grDoc.addToHistory(op);

			}

			grDoc.deselectAll();
			repaint();
			
		}
	}

	private void updateObjectInspector()
	{
		if (grDoc.getSelection().size() == 1)
		{
			ObjectInspector obji = MainFrame.getInstance().getObjectInspector();
			GShape shape = (GShape) grDoc.getSelection().get(0);
			obji.setTop((int)(shape.getMinPoint().y));
			obji.setLeft((int)(shape.getMinPoint().x));
			obji.setWidth((int) shape.getWidth());
			obji.setHeight((int) shape.getHeight());
		}

	}
}


