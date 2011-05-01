package geditor.elements;

import geditor.engine.tree.ObjectProperties;

import java.awt.*;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 10, 2003
 * Time: 10:15:37 AM
 */
public class GText extends GShape
{

	protected String text;


	/**
	 * these define the top left and the bottom right corner of the rectangle enclosing this text layout
	 */
	protected Point2DDouble topLeftPoint;
	protected Point2DDouble bottomRightPoint;

	public GText(Point2DDouble topLeftPoint, Point2DDouble bottomRightPoint, int z, String text)
	{
		super(z);

		this.text = text;
		this.topLeftPoint = topLeftPoint;
		this.bottomRightPoint = bottomRightPoint;

		internalScale();
	}

	public GText(Point2DDouble topLeftPoint, Point2DDouble bottomRightPoint, int z, Color color, String text)
	{
		this(topLeftPoint, bottomRightPoint, z, text);
		super.fgColor = color;
		this.text = text;
	}

	public GText(Point2DDouble topLeftPoint, Point2DDouble bottomRightPoint, int z, Color color, Color bckColor, String text)
	{
		this(topLeftPoint, bottomRightPoint, z, color, text);
		super.bgColor = bckColor;
		this.text = text;
	}

	public String getText()
	{
		return text;
	}

	public void setText(String text)
	{
		this.text = text;

/*
		textLayout = new TextLayout(text, new Font("Helvetica", 1, 20),
			new FontRenderContext(null, false, false));
		internalScale();
*/
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

		topLeftPoint.x += dx;
		topLeftPoint.y += dy;
		bottomRightPoint.x += dx;
		bottomRightPoint.y += dy;

		internalScale();
	}

	/**
	 * {@inheritDoc}
	 */
	public ObjectProperties getObjectProperties()
	{
		return new ObjectProperties(id, (int) topLeftPoint.x, (int) topLeftPoint.y, (int) getWidth(),
			(int) getHeight(), fgColor, bgColor, text);
	}

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

		internalScale();
	}

	/**
	 * Sets the top left corner of this text. Make sure that both the x and y coordinates of this point are
	 * lower than or equal to those of the bottom right point. Otherwise, the method will not change the top left
	 * point.
	 */
	public void setTopLeftPoint(Point2DDouble topLeftPoint)
	{
		if (topLeftPoint.getX() > bottomRightPoint.getX() || topLeftPoint.getY() > bottomRightPoint.getY())
		{
			return;
		}

		this.topLeftPoint = topLeftPoint;
		internalScale();
	}

	/**
	 * Sets the bottom right corner of this text. Make sure that both the x and y coordinates of this point are
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
		internalScale();
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
	 * Sets the both the top left and the bottom right corners of this text. Make sure that the top left point
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

		internalScale();
	}

	/**
	 * {@inheritDoc}
	 */
	public Object clone()
	{
		GText gText = (GText) super.clone();

		gText.text = text;

		gText.topLeftPoint = (Point2DDouble) topLeftPoint.clone();
		gText.bottomRightPoint = (Point2DDouble) bottomRightPoint.clone();


		gText.internalScale();

		return gText;
	}

	/**
	 * Scales the TextLayout contained by this object.
	 */
	private void internalScale()
	{
	}

	public String toString()
	{
		return "text " + id + " [" + topLeftPoint.x  + ", " + topLeftPoint.y + "] " +
						        + getWidth()  + ", " + getHeight() + "\"" + text + "\"";
			
	}

	public boolean insideObject(Point2DDouble p)
	{
		TextLayout textLayout = new TextLayout(text, new Font("Helvetica", 1, 20),
			new FontRenderContext(null, false, false));
		AffineTransform textAt = new AffineTransform();
		textAt.translate((float) topLeftPoint.x,
			(float) ((bottomRightPoint.y)));

		float alfaX = ((float) (bottomRightPoint.x - topLeftPoint.x)) / ((float) textLayout.getBounds().getWidth());
		float alfaY = ((float) (bottomRightPoint.y - topLeftPoint.y)) / ((float) textLayout.getBounds().getHeight());

		textAt.scale(alfaX, alfaY);
		Shape shape  = textLayout.getOutline(textAt);

		return shape.contains(p.x, p.y);
	}

	public void draw(Graphics2D graphics)
	{
		TextLayout textLayout = new TextLayout(text, new Font("Helvetica", 1, 20),
			new FontRenderContext(null, false, false));
		AffineTransform textAt = new AffineTransform();
		textAt.translate((float) topLeftPoint.x,
			(float) ((bottomRightPoint.y)));

		float alfaX = ((float) (bottomRightPoint.x - topLeftPoint.x)) / ((float) textLayout.getBounds().getWidth());
		float alfaY = ((float) (bottomRightPoint.y - topLeftPoint.y)) / ((float) textLayout.getBounds().getHeight());

		textAt.scale(alfaX, alfaY);
		Shape shape  = textLayout.getOutline(textAt);

		Stroke oldStroke = graphics.getStroke();
		graphics.setStroke(new BasicStroke(strokeWidth));
		graphics.setPaint(bgColor);
		graphics.fill(shape);
		graphics.setColor(fgColor);
		graphics.draw(shape);
		graphics.setStroke(oldStroke);

		if (selected) selectObject(graphics);
	}


	public void merge(GShape rs, GShape os, boolean keeplocalversion)
	{
		super.merge(rs, os, keeplocalversion);
		GText rt = (GText) rs;
		GText ot = (GText) os;

		if (bottomRightPoint.equals(ot.bottomRightPoint) ||
		            (!keeplocalversion && !rt.bottomRightPoint.equals(ot.bottomRightPoint)))
				bottomRightPoint = (Point2DDouble) rt.bottomRightPoint.clone();

		if (topLeftPoint.equals(ot.topLeftPoint) ||
		            (!keeplocalversion && !rt.topLeftPoint.equals(ot.topLeftPoint)))
				topLeftPoint = (Point2DDouble) rt.topLeftPoint.clone();

		if (text.equals(ot.getText()) || (!keeplocalversion && !rt.getText().equals(ot.getText())))
				text = rt.getText();
	}

	public boolean equals(Object obj)
	{
		if (!(super.equals(obj)))
		{
			return false;
		}

		if (!(obj instanceof GText))
		{
			return false;
		}

		GText text = (GText) obj;

		if (!(text.getBottomRightPoint().equals(getBottomRightPoint())))
		{
			return false;
		}

		if (!(text.getTopLeftPoint().equals(getTopLeftPoint())))
		{
			return false;
		}

		if (!(text.getText().equals(getText())))
		{
			return false;
		}

		return true;
	}
}