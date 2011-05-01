package geditor.elements;


import geditor.engine.tree.ObjectProperties;

import java.awt.*;
import java.awt.geom.Point2D;
import java.io.Serializable;
import java.rmi.server.UID;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 7, 2003
 * Time: 3:37:20 PM
 */
public abstract class GShape implements Serializable, Cloneable,
        Comparable
{
	public final static int SELECTED_RECT_DIM = 8;

	/**
	 * definies the width of the Stroke used when drawing selected objects
	 */
	protected float strokeWidth = 2.0f;

	/**
	 * the foreground and background colors to draw this object with
	 */
	protected Color fgColor;
	protected Color bgColor;

	/**
	 * the z-axis of this object (used to draw objects in some order on the screen)
	 */
	protected double z;

	/**
	 * should be initialized appropriately by each subclass that doesn't override the draw method
	 */
	protected transient Shape shape;

	/**
	 * this represents the parent of this GShpae in the graphical hierarcy
	 */
	protected GGroup parent;

	protected String id;

	public  boolean selected;



	// absolute reference point. used by the scale operation
	// they are initialy both 0, and can be modified by move
	public double rfx;
	public double rfy;





	public boolean isSelected()
	{
		return selected;
	}

	public void setSelected(boolean selected)
	{
		this.selected = selected;
	}

	public GShape(double z)
	{
		this.shape = null;

		this.fgColor = Color.black;
		this.bgColor = Color.white;

		this.z = z;

		generateId();

		rfx = 0; rfy = 0;

	}

	public GShape(double z, Color fgColor)
	{
		this(z);
		this.fgColor = fgColor;

		rfx = 0; rfy = 0;
	}

	public GShape(double z, Color fgColor, Color bgColor)
	{
		this(z);
		this.fgColor = fgColor;
		this.bgColor = bgColor;

		rfx = 0; rfy = 0;

	}

	private void generateId()
	{
		UID uid = new UID();
		id = uid.toString();
	}

	public void setBgColor(Color bgColor)
	{
		this.bgColor = bgColor;
	}

	public void setFgColor(Color fgColor)
	{
		this.fgColor = fgColor;
	}

	public void setStrokeWidth(float strokeWidth)
	{
		this.strokeWidth = strokeWidth;
	}

	protected void setShape(Shape shape)
	{
		this.shape = shape;
	}

	public void setZ(double z)
	{
		this.z = z;
	}

	public void setParent(GGroup parent)
	{
		this.parent = parent;
	}

	public Color getBgColor()
	{
		return bgColor;
	}

	public Color getFgColor()
	{
		return fgColor;
	}

	public float getStrokeWidth()
	{
		return strokeWidth;
	}

	public double getZ()
	{
		return z;
	}

	public String getId()
	{
		return id;
	}

	public GGroup getParent()
	{
		return parent;
	}

	/**
	 * Returns the width of this shape.
	 */
	public abstract double getWidth();

	/**
	 * Returns the height of this shape.
	 */
	public abstract double getHeight();

	/**
	 * Returns the bottom right point of the smallest rectangle that completely encloses this shape.
	 */
	public abstract Point2DDouble
	        getMaxPoint();

	public double getRfx()
	{
		return rfx;
	}

	public void setRfx(double rfx)
	{
		this.rfx = rfx;
	}

	public double getRfy()
	{
		return rfy;
	}

	public void setRfy(double rfy)
	{
		this.rfy = rfy;
	}

	/**
	 * Returns the top left point of the smallest rectangle that completely encloses this shape.
	 */
	public abstract Point2DDouble getMinPoint();

	/**
	 * Moves this object from its current position with <code>dx</code> on the x axis and with <code>dy</code> on
	 * the y axis.
	 */
	public void moveWith(double dx, double dy)
	{
		rfx += dx;
		rfy += dy;
	}

	/**
	 * Returns this shape as an ObjectProperties object.
	 */
	public abstract ObjectProperties getObjectProperties();

	
	/**
	 * Draws this shape on the <code>graphics</code> canvas.
	 */
	public void draw(Graphics2D graphics)
	{
		Stroke oldStroke = graphics.getStroke();
		graphics.setStroke(new BasicStroke(strokeWidth));
		graphics.setPaint(bgColor);
		graphics.fill(shape);
		graphics.setColor(fgColor);
		graphics.draw(shape);
		graphics.setStroke(oldStroke);

		if (selected) selectObject(graphics);
	}

	/**
	 * Returns true if <code>p</code> is contained in this object and false otherwise.
	 */
	public boolean insideObject(Point2DDouble p)
	{
		return shape.contains(p.x,  p.y);
	}

	/**
	 * Draws a visual selection of this object. This consists of drawing four small ovals at the four (or two, in
	 * case of lines) corners of the shape.
	 */
	public void selectObject(Graphics2D graphics)
	{
		double x1 = getMinPoint().x;
		double x2 = getMaxPoint().x;
		double y1 = getMinPoint().y;
		double y2 = getMaxPoint().y;

		graphics.setPaint(Color.gray);
		graphics.setStroke(new BasicStroke(0.5f));

		graphics.fillOval((int)(x1 - (SELECTED_RECT_DIM / 2)), (int)(y1 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		graphics.fillOval((int)(x2 - (SELECTED_RECT_DIM / 2)), (int)(y2 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		graphics.fillOval((int)(x1 - (SELECTED_RECT_DIM / 2)), (int)y2 - (SELECTED_RECT_DIM / 2), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		graphics.fillOval((int)(x2 - (SELECTED_RECT_DIM / 2)), (int)(y1 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);

		graphics.setColor(Color.black);

		graphics.drawOval((int)(x1 - (SELECTED_RECT_DIM / 2)), (int)(y1 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		graphics.drawOval((int)(x2 - (SELECTED_RECT_DIM / 2)), (int)(y2 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		graphics.drawOval((int)(x1 - (SELECTED_RECT_DIM / 2)), (int)(y2 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		graphics.drawOval((int)(x2 - (SELECTED_RECT_DIM / 2)), (int)(y1 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
	}

	/**
	 * Returns true if this is a basic graphical object and, consequently, is always a leaf in the tree representing
	 * the graphical hierarchy. By default, returns true. All classes that represent containers of basic objects
	 * should override this and return false.
	 */
	public boolean isLeaf()
	{
		return true;
	}

	public abstract void scale(double refx, double refy, double tx, double ty);

	/**
	 * {@inheritDoc}
	 */
	public Object clone()
	{
		GShape gShape = null;

		try
		{
			gShape = (GShape) super.clone();
		}
		catch (CloneNotSupportedException e)
		{
			// not the case
		}

		gShape.fgColor = fgColor;
		gShape.bgColor = bgColor;
		gShape.parent = parent;

		return gShape;
	}

	 public int compareTo(Object o)
	 {
		 GShape shape = (GShape)o;
		 if (getZ() == shape.getZ()) return getId().compareTo(shape.getId());
		 if (getZ() < shape.getZ()) return 1;
		 return -1;
	 }

	// return null if (x, y) is not in one of the scaling dots
	public Point2DDouble getScalingReferencePoint(double x, double y)
	{
		double minx, miny, maxx, maxy;
		minx = getMinPoint().x;
		miny = getMinPoint().y;
		maxx = getMaxPoint().x;
		maxy = getMaxPoint().y;

		if (Math.abs(x - minx) <= SELECTED_RECT_DIM / 2 && Math.abs(y - miny) <= SELECTED_RECT_DIM / 2)
				return  new Point2DDouble(maxx, maxy);
		if (Math.abs(x - maxx) <= SELECTED_RECT_DIM / 2 && Math.abs(y - maxy) <= SELECTED_RECT_DIM / 2)
				return  new Point2DDouble(minx, miny);
		if (Math.abs(x - maxx) <= SELECTED_RECT_DIM / 2 && Math.abs(y - miny) <= SELECTED_RECT_DIM / 2)
				return  new Point2DDouble(minx, maxy);
		if (Math.abs(x - minx) <= SELECTED_RECT_DIM / 2 && Math.abs(y - maxy) <= SELECTED_RECT_DIM / 2)
				return  new Point2DDouble(maxx, miny);

		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	public boolean existsId(String id)
	{
		return getId().equals(id);
	}

	public void setId(String id)
	{
		this.id = id;
	}

	public void clearSelection()
	{
		selected = false;
	}

	public void selectAllGroups()
	{

	}

	public ArrayList getLeafs()
	{
		ArrayList result = new ArrayList();
		result.add(getId());
		return result;
	}
}