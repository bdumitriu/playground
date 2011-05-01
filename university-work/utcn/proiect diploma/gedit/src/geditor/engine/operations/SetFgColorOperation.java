package geditor.engine.operations;

import geditor.elements.*;

import java.awt.*;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 2:15:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class SetFgColorOperation
	extends Operation
{
	Color color;


	ArrayList shapeInv = new ArrayList();
	ArrayList colorInv = new ArrayList();


	public SetFgColorOperation(Color color, String shapeId)
	{
		this.color = color;
		this.shapeId = shapeId;
	}

	public Color getColor()
	{
		return color;
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		for (int  i = 0; i < shapeInv.size(); i++)
		{
			SetFgColorOperation fgo = new SetFgColorOperation((Color)colorInv.get(i), (String)shapeInv.get(i));

			fgo.shapeInv = new ArrayList();
			fgo.colorInv = new ArrayList();
			fgo.shapeInv.add(shapeInv.get(i));
			fgo.colorInv.add(color);

			result.add(fgo);
		}

		return result;
	}


	private void calcInv(GShape shape)
	{

		if (shape instanceof GGroup)
		{
			GGroup g = (GGroup) shape;
			for (int i = 0; i < g.getNrChildren(); i++)
				calcInv((GShape) g.getChildren().get(i));
		}
		else
		{
			shapeInv.add(shape.getId());
			colorInv.add(shape.getFgColor());
		}

	}

		


	public void applyTo(GRootGroup g)
	{

		super.applyTo(g);		

		GShape shape = g.getShape(shapeId);

		shapeInv = new ArrayList();
		colorInv = new ArrayList();


		if (shape != null)
		{
			calcInv(shape);
			shape.setFgColor(color);
		}
	}

	public String toString()
	{
		return "SetFgColor" + " " + shapeId + " " + color;
	}	
}
