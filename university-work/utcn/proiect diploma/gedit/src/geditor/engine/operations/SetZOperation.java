package geditor.engine.operations;

import geditor.elements.GRootGroup;
import geditor.elements.GShape;
import geditor.elements.GText;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Apr 20, 2004
 * Time: 7:42:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class SetZOperation extends  Operation
{
	private double z;
	private double oldz;

	public SetZOperation(String shapeId, double z)
	{
		this.z = z;
		this.shapeId = shapeId;
		this.oldz = 0;
	}

	public double getZ()
	{
		return z;
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		SetZOperation setzo = new SetZOperation(shapeId, oldz);
		setzo.oldz = z;
		result.add(setzo);
		return result;
	}

	public void applyTo(GRootGroup g)
	{
		super.applyTo(g);		

		GShape shape = g.getShape(shapeId);
		if (shape == null) return;
		oldz = shape.getZ();
		shape.setZ(z);
	}

	public String toString()
	{
		return "SetZ" + " " + shapeId + " " + z;
	}

}
