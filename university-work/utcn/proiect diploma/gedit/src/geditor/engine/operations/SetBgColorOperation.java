package geditor.engine.operations;

import geditor.elements.GRootGroup;
import geditor.elements.GShape;
import geditor.elements.GGroup;

import java.awt.*;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 2:35:57 PM
 * To change this template use File | Settings | File Templates.
 */
public class SetBgColorOperation
	extends Operation
{
	Color color;

	ArrayList shapeInv = new ArrayList();
	ArrayList colorInv = new ArrayList();



	public Color getColor()
	{
		return color;
	}



	public SetBgColorOperation(Color color, String shapeId)
	{
		this.color = color;
		this.shapeId = shapeId;
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
			colorInv.add(shape.getBgColor());
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
			shape.setBgColor(color);
		}

	}

	public String toString()
	{
		return "SetBgColor" + " " + shapeId + " "  + color;
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		for (int  i = 0; i < shapeInv.size(); i++)
		{
			SetBgColorOperation bgo = new SetBgColorOperation((Color)colorInv.get(i), (String)shapeInv.get(i));

			bgo.shapeInv = new ArrayList();
			bgo.colorInv = new ArrayList();
			bgo.shapeInv.add(shapeInv.get(i));
			bgo.colorInv.add(color);

			result.add(bgo);
		}

		return result;
	}

}
