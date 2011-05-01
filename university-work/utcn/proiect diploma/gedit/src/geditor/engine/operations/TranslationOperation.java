package geditor.engine.operations;

import geditor.elements.GRootGroup;
import geditor.elements.GShape;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 3:22:05 PM
 * To change this template use File | Settings | File Templates.
 */
public class TranslationOperation
        extends Operation
{
	private double dx;
	private double dy;

	public TranslationOperation(String shapeId, double dx, double dy)
	{
		this.shapeId = shapeId;
		this.dx = dx;
		this.dy = dy;
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		result.add(new TranslationOperation(shapeId, -dx, -dy));
		return result;
	}


	public void applyTo(GRootGroup g)
	{
		super.applyTo(g);
		
		GShape shape = g.getShape(shapeId);
		if (shape == null) return;
		shape.moveWith(dx, dy);
	}

	public  String toString()
	{
		return "translation " + shapeId + " " + dx + " " + dy;
	}

	public double getDx()
	{
		return dx;
	}

	public void setDx(double dx)
	{
		this.dx = dx;
	}

	public double getDy()
	{
		return dy;
	}

	public void setDy(double dy)
	{
		this.dy = dy;
	}
}
