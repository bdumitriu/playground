package geditor.engine.operations;

import geditor.elements.*;

import java.util.ArrayList;
import java.net.UnknownHostException;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 3:39:41 PM
 * To change this template use File | Settings | File Templates.
 */
public class GroupOperation
        extends Operation
{
	private String parentId;
	private ArrayList children;
	private String groupId;
	private GRootGroup shapeGroup;

	private double objrefx = 0;
	private double objrefy = 0;


	public GroupOperation(String parentid, ArrayList children)
	{
		this.parentId = parentid;


		this.children = (ArrayList)children.clone();
		for (int i = 0; i < this.children.size(); i++)
				this.children.set(i, ((GShape)this.children.get(i)).clone());

		shapeGroup = new GRootGroup();
		groupId = null;
		for (int i = 0; i < this.children.size(); i++)
			try
			{
				shapeGroup.addShapeWithChildren(shapeGroup.getId(), (GShape)(this.children.get(i)) );
			} catch (TreeInconsistencyException e)
			{
				e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			}

		this.children = shapeGroup.getChildren();
		this.shapeId = "";
	}


	public String getGroupId()
	{
		return groupId;
	}

	public void setGroupId(String groupId)
	{
		this.groupId = groupId;
	}

	public ArrayList invert()
	{
		UngroupOperation ugo = new UngroupOperation(parentId, groupId, children);
		ugo.setShapeId(getShapeId());
		ugo.setGroupId(getShapeId());
		ugo.setObjrefx(objrefx);
		ugo.setObjrefy(objrefy);
		ArrayList result = new ArrayList();
		result.add(ugo);
		return result;
	}


	public Object clone()
	{
		GroupOperation dolly = (GroupOperation) super.clone();
		dolly.shapeGroup = (GRootGroup) dolly.shapeGroup.clone();
        dolly.children = dolly.shapeGroup.getChildren();
		return dolly;
	}


	public void applyTo(GRootGroup g)
	{
		GGroup parent; //= (GGroup) g.getShape(parentId);
//		if (parent == null) return;
		GGroup group = new GGroup();
		group.setZ(g.getMinZ() - 1);
		group.setRfx(objrefx);
		group.setRfy(objrefy);

		if (groupId == null) groupId = group.getId();
			else
		{
			group.setId(groupId);
		}
		shapeId = group.getId();



		parent = null;
		for (int i = 0; i < children.size(); i++)
		{
			//GShape shape = (GShape) parent.findChild( ((GShape)children.get(i)).getId());
			GShape  shape = (GShape) g.getShape(((GShape)children.get(i)).getId());
			if (shape == null) continue;
			if (parent == null) parent = shape.getParent();
			else
				if (parent != shape.getParent()) return;
			parent.deleteChild(shape);
			group.addChild(shape);
			shape.setParent(group);
		}
		if (parent == null) return;
		parentId = parent.getId();
		try
		{
			g.addShape(parentId, (GShape) group);
		} catch (TreeInconsistencyException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	public String toString()
	{
		String result =  "group " + getShapeId() + " (";
		for (int i = 0; i < children.size(); i++)
			result += ((GShape)children.get(i)).getId();
		return result;
	}

	public ArrayList getChildren()
	{
		return children;
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
}
