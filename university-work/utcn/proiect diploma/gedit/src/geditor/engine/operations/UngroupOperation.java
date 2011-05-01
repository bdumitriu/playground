package geditor.engine.operations;

import geditor.elements.*;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 3:47:11 PM
 * To change this template use File | Settings | File Templates.
 */
public class UngroupOperation
        extends Operation
{
	private String parentId;
	private String groupId;
	private ArrayList children;
	private GRootGroup shapeGroup;


	private double objrefx = 0;
	private double objrefy = 0;

	public ArrayList getChildren()
	{
		return children;
	}

	public UngroupOperation(String parentId, String groupId, ArrayList children)
	{
		this.parentId = parentId;
		this.groupId = groupId;
		this.children = (ArrayList)children.clone();
		for (int i = 0; i < this.children.size(); i++)
				this.children.set(i, ((GShape)this.children.get(i)).clone());
		shapeGroup = new GRootGroup();
		for (int i = 0; i < this.children.size(); i++)
			try
			{
				shapeGroup.addShapeWithChildren(shapeGroup.getId(), (GShape)(this.children.get(i)));
			} catch (TreeInconsistencyException e)
			{
				e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			}

		shapeId = groupId;
	}

	public UngroupOperation(GGroup group)
	{
		this.children = new ArrayList();
		this.groupId = group.getId();
		this.parentId = group.getId();
		for (int i = 0; i < group.getNrChildren(); i++)
			this.children.add(group.getId());
	}

	public ArrayList invert()
	{
		GroupOperation go =  new GroupOperation(parentId, children);
		go.setShapeId(getShapeId());
		go.setGroupId(getGroupId());
		go.setObjrefx(objrefx);
		go.setObjrefx(objrefy);		
		ArrayList result = new ArrayList();
		result.add(go);
		return result;

	}


	public double getObjrefx()
	{
		return objrefx;
	}

	public void setObjrefx(double objrefx)
	{
		this.objrefx = objrefx;
	}

	public double getObjrefy()
	{
		return objrefy;
	}

	public void setObjrefy(double objrefy)
	{
		this.objrefy = objrefy;
	}

	public void applyTo(GRootGroup g)
	{

		super.applyTo(g);

		GGroup group = (GGroup) g.getShape(groupId);
		if (group == null) return;


		g.deleteShape(group.getId());
	}

	public String toString()
	{
		String result =  "ungroup " + getShapeId() + " (";
		for (int i = 0; i < children.size(); i++)
			result += children.get(i);
		return result;
	}

	public String getParentId()
	{
		return parentId;
	}

	public String getGroupId()
	{
		return groupId;
	}

	public void setGroupId(String groupId)
	{
		this.groupId = groupId;
	}
}
