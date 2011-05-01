package geditor.elements;

import geditor.engine.tree.ObjectProperties;

import java.awt.*;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Dimension2D;
import java.io.ObjectInputStream;
import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 3, 2003
 * Time: 2:44:03 PM
 */
public class GEllipse extends GShape
{
	private transient  Ellipse2D.Double ellipse;

	/**
	 * these define the top left and the bottom right corner of the smallest rectangle enclosing this ellipse
	 */
	protected Point2DDouble topLeftPoint;
	protected Point2DDouble bottomRightPoint;

	public GEllipse(Point2DDouble topLeftPoint, Point2DDouble bottomRightPoint, int z)
	{
		super(z);

		this.topLeftPoint = topLeftPoint;
		this.bottomRightPoint = bottomRightPoint;

		ellipse = new Ellipse2D.Double(topLeftPoint.x, topLeftPoint.y, getWidth(), getHeight());

		shape = ellipse;
	}

	public GEllipse(Point2DDouble topLeftPoint, Point2DDouble bottomRightPoint, int z, Color fgColor)
	{
		this(topLeftPoint, bottomRightPoint, z);
		super.fgColor = fgColor;
	}

	public GEllipse(Point2DDouble topLeftPoint, Point2DDouble bottomRightPoint, int z, Color fgColor, Color bgColor)
	{
		this(topLeftPoint, bottomRightPoint, z, fgColor);
		super.bgColor = bgColor;
	}

	/**
	 * {@inheritDoc}
	 */
	public double getWidth()
	{
		return Math.abs(bottomRightPoint.x - topLeftPoint.x);
	}

	/**
	 * {@inheritDoc}
	 */
	public double getHeight()
	{
		return Math.abs(bottomRightPoint.y - topLeftPoint.y);
	}

	/**
	 * {@inheritDoc}
	 */
	public Point2DDouble getMaxPoint()
	{
		return bottomRightPoint;
	}

	/**
	 * {@inheritDoc}
	 */
	public Point2DDouble getMinPoint()
	{
		return topLeftPoint;
	}

	/**
	 * {@inheritDoc}
	 */
	public void moveWith(double dx, double dy)
	{
		super.moveWith(dx, dy);
		topLeftPoint.setLocation(topLeftPoint.getX() + dx, topLeftPoint.getY() + dy);

		bottomRightPoint.setLocation(bottomRightPoint.getX() + dx, bottomRightPoint.getY() + dy);
		ellipse.setFrame(topLeftPoint.x, topLeftPoint.y, getWidth(), getHeight());
	}

	/**
	 * {@inheritDoc}
	 */
	public ObjectProperties getObjectProperties()
	{
		return new ObjectProperties(id, (int) topLeftPoint.x, (int) topLeftPoint.y, (int) getWidth(),
			(int) getHeight(), fgColor, bgColor, null);
	}

	/**
	 * {@inheritDoc}
	 * TODO: rewrite this method
	 */

	public void scale(double refx, double refy, double tx, double ty)
	{
		topLeftPoint.x = refx + (topLeftPoint.x - refx) * tx;
		topLeftPoint.y = refy + (topLeftPoint.y - refy) * ty;

		bottomRightPoint.x = refx + (bottomRightPoint.x - refx) * tx;
		bottomRightPoint.y = refy + (bottomRightPoint.y - refy) * ty;

		if (topLeftPoint.x > bottomRightPoint.x)
		{
			double aux = topLeftPoint.x;
			topLeftPoint.x = bottomRightPoint.x;
			bottomRightPoint.x = aux;
		}

		if (topLeftPoint.y > bottomRightPoint.y)
		{
			double aux = topLeftPoint.y;
			topLeftPoint.y = bottomRightPoint.y;
			bottomRightPoint.y = aux;
		}
		if (topLeftPoint.x == bottomRightPoint.x) bottomRightPoint.x++;
		if (topLeftPoint.y == bottomRightPoint.y) bottomRightPoint.y++;
		ellipse.setFrame(topLeftPoint.x, topLeftPoint.y, getWidth(), getHeight());

	}


	/**
	 * Sets the top left corner of this ellipse. Make sure that both the x and y coordinates of this point are lower
	 * than or equal to those of the bottom right point. Otherwise, the method will not change the top left point.
	 */
	public void setTopLeftPoint(Point2DDouble topLeftPoint)
	{
		if (topLeftPoint.getX() > bottomRightPoint.getX() || topLeftPoint.getY() > bottomRightPoint.getY())
		{
			return;
		}

		this.topLeftPoint = topLeftPoint;
		ellipse.setFrame(this.topLeftPoint.x, this.topLeftPoint.y, getWidth(), getHeight());
	}

	/**
	 * Sets the bottom right corner of this ellipse. Make sure that both the x and y coordinates of this point are
	 * greater than or equal to those of the top left point. Otherwise, the method will not change the bottom right
	 * point.
	 */
	public void setBottomRightPoint(Point2DDouble bottomRightPoint)
	{
		if (bottomRightPoint.getX() < topLeftPoint.getX() || bottomRightPoint.getY() < topLeftPoint.getY())
		{
			return;
		}

		this.bottomRightPoint = bottomRightPoint;
		ellipse.setFrame(topLeftPoint.x, topLeftPoint.y, getWidth(), getHeight());
	}

	/**
	 * Sets the both the top left and this bottom right corners of the ellipse. Make sure that the top left point
	 * coordinates are <b>both</b> lower than or equal to those of the bottom right ones. Otherwise, the method will
	 * not change either of them.
	 */
	public void setPoints(Point2DDouble topLeftPoint, Point2DDouble bottomRightPoint)
	{
		if (topLeftPoint.getX() > bottomRightPoint.getX() || topLeftPoint.getY() > bottomRightPoint.getY())
		{
			return;
		}

		this.topLeftPoint = topLeftPoint;
		this.bottomRightPoint = bottomRightPoint;

		ellipse.setFrame(topLeftPoint.x,  topLeftPoint.y, getWidth(), getHeight());
	}

	public Point2DDouble getTopLeftPoint()
	{
		return topLeftPoint;
	}

	public Point2DDouble getBottomRightPoint()
	{
		return bottomRightPoint;
	}

	/**
	 * {@inheritDoc}
	 */
	public Object clone()
	{
		GEllipse gEllipse = (GEllipse) super.clone();

		gEllipse.ellipse = (Ellipse2D.Double) ellipse.clone();
		gEllipse.topLeftPoint = (Point2DDouble) topLeftPoint.clone();
		gEllipse.bottomRightPoint = (Point2DDouble) bottomRightPoint.clone();
		gEllipse.shape = gEllipse.ellipse;

		return gEllipse;
	}

	public String toString()
	{
		return "ellipse " + id + " [" + topLeftPoint.x  + ", " + topLeftPoint.y + "] " +
					        + getWidth()  + ", " + getHeight();
	}


	private void readObject(ObjectInputStream in) throws IOException , ClassNotFoundException
	{
		in.defaultReadObject();
		ellipse = new Ellipse2D.Double(topLeftPoint.x, topLeftPoint.y, bottomRightPoint.x - topLeftPoint.x,
		                                   bottomRightPoint.y - topLeftPoint.y);


		shape = ellipse;
	}

}