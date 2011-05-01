package geditor.elements;

import java.awt.geom.Point2D;
import java.io.Serializable;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 25, 2004
 * Time: 10:57:22 AM
 * To change this template use File | Settings | File Templates.
 */
public class Point2DDouble
                            implements Serializable, Cloneable

{
	public double x;
	public double y;

	public double getX()
	{
		return x;
	}

	public void setX(double x)
	{
		this.x = x;
	}

	public double getY()
	{
		return y;
	}

	public void setY(double y)
	{
		this.y = y;
	}

	public Point2DDouble(double x, double y)
	{
		this.x = x;
		this.y = y;
	}

	public void setLocation(double x, double y)
	{
		this.x = x;
		this.y = y;

	}

	public Object clone()
	{
		try
		{
			return super.clone();    //To change body of overridden methods use File | Settings | File Templates.
		} catch (CloneNotSupportedException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		return null;
	}
}
