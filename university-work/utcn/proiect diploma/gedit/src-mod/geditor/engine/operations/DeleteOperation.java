package geditor.engine.operations;

import geditor.elements.GShape;
import geditor.elements.GRootGroup;
import geditor.elements.TreeInconsistencyException;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 2:51:25 PM
 * To change this template use File | Settings | File Templates.
 */
public class DeleteOperation
        extends Operation
{
	private String parentId;
	private GRootGroup shapeGroup;
	private GShape shape;

	public GRootGroup getShapeGroup()
	{
		return shapeGroup;
	}

	public DeleteOperation(String parentId, GShape shape)
	{
		this.parentId = parentId;
		this.shape = (GShape)shape.clone();
		this.shapeId = shape.getId();

		shapeGroup = new GRootGroup();
		try
		{
			shapeGroup.addShapeWithChildren(shapeGroup.getId(), this.shape);
		} catch (TreeInconsistencyException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	public String getParentId()
	{
		return parentId;
	}

	public GShape getShape()
	{
		return shape;
	}


	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		result.add(new CreateOperation(parentId, shape));
		return result;
	}

	public Object clone()
	{
		DeleteOperation dolly;
		dolly = (DeleteOperation)super.clone();
		dolly.shape = (GShape)shape.clone();

		dolly.shapeGroup = new GRootGroup();
		try
		{
			dolly.shapeGroup.addShapeWithChildren(dolly.shapeGroup.getId(), dolly.shape);
		} catch (TreeInconsistencyException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}


		return dolly;
	}


	public void applyTo(GRootGroup g)
	{

		g.deleteShapeWithChildren(shape.getId());
	}

	public String toString()
	{
		return "delete " + shape;
	}


}
