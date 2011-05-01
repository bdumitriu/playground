package geditor.elements;

import geditor.engine.tree.ObjectProperties;

import javax.sound.sampled.Line;
import java.awt.*;
import java.awt.geom.Point2D;
import java.awt.geom.Line2D;
import java.awt.geom.Ellipse2D;
import java.io.ObjectInputStream;
import java.io.IOException;


/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 24, 2003
 * Time: 5:14:29 PM
 */


public class GLine extends GShape
{
	private  transient Line2D.Double line;
	/**
	 * these define the two ends of the line
	 */
	protected Point2DDouble startPoint;
	protected Point2DDouble endPoint;

	/**
	 * the points that represent the upper left and lower right corners of the line
	 */
	protected Point2DDouble minPoint;
	protected Point2DDouble maxPoint;

	public GLine(Point2DDouble startPoint, Point2DDouble endPoint, int z)
	{
		super(z);

		line = new Line2D.Double(startPoint.getX(), startPoint.getY(), endPoint.getX(), endPoint.getY());

		this.startPoint = startPoint;
		this.endPoint = endPoint;

		super.bgColor = null;

		updateMinMaxPoints();

		shape = line;
	}

	public GLine(Point2DDouble startPoint, Point2DDouble endPoint, int z, Color color)
	{
		this(startPoint, endPoint, z);

		super.fgColor = color;
		super.bgColor = null;
	}

	/**
	 * {@inheritDoc}
	 */
	public double getWidth()
	{
		return Math.abs(startPoint.x - endPoint.x);
	}

	/**
	 * {@inheritDoc}
	 */
	public double getHeight()
	{
		return Math.abs(startPoint.y - endPoint.y);
	}

	/**
	 * {@inheritDoc}
	 */
	public Point2DDouble getMaxPoint()
	{
		return maxPoint;
	}

	/**
	 * {@inheritDoc}
	 */
	public Point2DDouble getMinPoint()
	{
		return minPoint;
	}

	/**
	 * {@inheritDoc}
	 */
	public void moveWith(double dx, double dy)
	{
		startPoint.setLocation(startPoint.getX() + dx, startPoint.getY() + dy);
		endPoint.setLocation(endPoint.getX() + dx, endPoint.getY() + dy)  ;

		line.setLine(startPoint.x, startPoint.y, endPoint.x, endPoint.y  );


		updateMinMaxPoints();
	}

	/**
	 * {@inheritDoc}
	 */
	public ObjectProperties getObjectProperties()
	{
		return new ObjectProperties(id, (int) getMinPoint().x, (int) getMinPoint().y, (int) getWidth(),
			(int) getHeight(), fgColor, bgColor, null);
	}

	public void setStartPoint(Point2DDouble startPoint)
	{
		this.startPoint = startPoint;
		line.setLine(startPoint.x, startPoint.y, endPoint.x, endPoint.y  );

		updateMinMaxPoints();
	}

	public void setEndPoint(Point2DDouble endPoint)
	{
		this.endPoint = endPoint;
		line.setLine(startPoint.x, startPoint.y, endPoint.x, endPoint.y  );

		updateMinMaxPoints();
	}

	public Point2DDouble getStartPoint()
	{
		return startPoint;
	}

	public Point2DDouble getEndPoint()
	{
		return endPoint;
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean insideObject(Point2DDouble p)
	{
		return line.intersects(p.x-2, p.y-2, 4, 4);
	}

	protected void updateMinMaxPoints()
	{
		minPoint = minPoint();
		maxPoint = maxPoint();
	}

	private Point2DDouble minPoint()
	{
		double minx = Math.min(startPoint.x, endPoint.x);
		double miny = Math.min(startPoint.y, endPoint.y);

		return new Point2DDouble(minx, miny);
	}

	private Point2DDouble maxPoint()
	{
		double maxx = Math.max(startPoint.x, endPoint.x);
		double maxy = Math.max(startPoint.y, endPoint.y);

		return new Point2DDouble(maxx, maxy);
	}

	/**
	 * {@inheritDoc}
	 */
	public void selectObject(Graphics2D g2)
	{
		double x1 = startPoint.x;
		double x2 = endPoint.x;
		double y1 = startPoint.y;
		double y2 = endPoint.y;

		g2.setPaint(Color.gray);
		g2.setStroke(new BasicStroke(0.5f));

		g2.fillOval((int) (x1 - (SELECTED_RECT_DIM / 2)), (int)(y1 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		g2.fillOval((int)(x2 - (SELECTED_RECT_DIM / 2)), (int)(y2 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);

		g2.setColor(Color.black);

		g2.drawOval((int)(x1 - (SELECTED_RECT_DIM / 2)), (int)(y1 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		g2.drawOval((int)(x2 - (SELECTED_RECT_DIM / 2)), (int) (y2 - (SELECTED_RECT_DIM / 2)), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
	}

	public void scale(double refx, double refy, double tx, double ty)
	{
		startPoint.x = refx - (refx - startPoint.x) * tx;
		startPoint.y = refy - (refy - startPoint.y) * ty;

		endPoint.x = refx - (refx - endPoint.x) * tx;
		endPoint.y = refy - (refy - endPoint.y) * ty;

		if (startPoint.x == endPoint.x) endPoint.x++;
		if (startPoint.y == endPoint.y) endPoint.y++;

		line.setLine(startPoint.x, startPoint.y, endPoint.x, endPoint.y  );
		updateMinMaxPoints();
	}

	/**
	 * {@inheritDoc}
	 */
	public Object clone()
	{
		GLine gLine = (GLine) super.clone();


		gLine.startPoint = (Point2DDouble) startPoint.clone();
		gLine.endPoint = (Point2DDouble) endPoint.clone();
		gLine.line = (Line2D.Double) line.clone();
        gLine.shape = gLine.line;

		return gLine;
	}

	// return null if (x, y) is not in one of the scaling dots
	public Point2DDouble getScalingReferencePoint(double x, double y)
	{
		if ((Math.abs(x - startPoint.x) < SELECTED_RECT_DIM / 2) &&
		        (Math.abs(y - startPoint.y) < SELECTED_RECT_DIM / 2))
						return (Point2DDouble)endPoint.clone();

		if ((Math.abs(x - endPoint.x) < SELECTED_RECT_DIM / 2) &&
		        (Math.abs(y - endPoint.y) < SELECTED_RECT_DIM / 2))
						return (Point2DDouble)startPoint.clone();


		return null;  //To change body of implemented methods use File | Settings | File Templates.

	}

	public String toString()
	{
		return "line [" + startPoint.x  + ", " + startPoint.y + "] " +
					   "[" + endPoint.x  + ", " + endPoint.y + "]";

	}

	private void readObject(ObjectInputStream in) throws IOException , ClassNotFoundException
	{
		in.defaultReadObject();
		line = new Line2D.Double(startPoint.x, startPoint.y, endPoint.x ,
		                                   endPoint.y);


		shape = line;
	}

	public void merge(GShape rs, GShape os, boolean keeplocalversion)
	{
		//super.merge(rs, os, keeplocalversion);


		if (getFgColor().equals(os.getFgColor()) ||  (!keeplocalversion && !rs.getFgColor().equals(os.getFgColor())))
				setFgColor(rs.getFgColor());

		if (z == os.z ||  (!keeplocalversion && rs.z != os.z))
				z  = rs.z;
		


		GLine rl = (GLine) rs;
		GLine ol = (GLine) os;

		if (startPoint.equals(ol.startPoint) ||
		            (!keeplocalversion && !rl.startPoint.equals(ol.startPoint)))
				startPoint = (Point2DDouble) rl.startPoint.clone();

		if (endPoint.equals(ol.endPoint) ||
		            (!keeplocalversion && !rl.endPoint.equals(ol.endPoint)))
				endPoint = (Point2DDouble) rl.endPoint.clone();

		line.setLine(startPoint.x, startPoint.y, endPoint.x, endPoint.y  );		

		updateMinMaxPoints();
	}

	public boolean equals(Object obj)
	{
		if (!(super.equals(obj)))
		{
			return false;
		}

		if (!(obj instanceof GLine))
		{
			return false;
		}

		GLine line = (GLine) obj;

		if (!(line.getStartPoint().equals(getStartPoint())))
		{
			return false;
		}

		if (!(line.getEndPoint().equals(getEndPoint())))
		{
			return false;
		}

		return true;
	}
}