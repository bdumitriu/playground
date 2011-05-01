package geditor.engine.operations;

import geditor.elements.GShape;
import geditor.elements.GRootGroup;
import geditor.elements.TreeInconsistencyException;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 2:41:33 PM
 * To change this template use File | Settings | File Templates.
 */
public class CreateOperation
        extends Operation
{
	private String parentId;
	private GShape shape;

	public GShape getShape()
	{
		return shape;
	}

	public CreateOperation(String parentId, GShape shape)
	{
		this.parentId = parentId;
		this.shape = (GShape) shape.clone();
		this.shape.setParent(null);
		shapeId = shape.getId();
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		result.add(new DeleteOperation(parentId, shape));
		return result;
	}

	public Object clone()
	{
		CreateOperation dolly = (CreateOperation)super.clone();
		dolly.shape = (GShape)dolly.shape.clone();
		return dolly;
	}


	public void applyTo(GRootGroup g)
	{
		super.applyTo(g);
		try
		{
			g.addShapeWithChildren(parentId, (GShape) shape.clone());

		} catch (TreeInconsistencyException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	public String toString()
	{
		return "create " + shape;
	}

	public String getParentId()
	{
		return parentId;
	}
}
