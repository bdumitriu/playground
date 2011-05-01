package geditor.elements;

import java.util.Hashtable;
import java.util.ArrayList;

/**
 * This class represents the top level group of the graphical document. It differs from a normal GGroup in that it
 * contains an extra hashtable with all the shapes of the graphical tree.
 * <br /><br />
 * Date: Mar 18, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class GRootGroup extends GGroup
{
	private Hashtable shapes;

	public GRootGroup()
	{
		super();

		id = "000";
		shapes = new Hashtable();
		shapes.put(getId(), this);
	}

	/**
	 * Adds <code>shape</code> to the hashtable, adds shape to the group identified by <code>parentId</code>. If you
	 * also want all the children of <code>shape</code> (if it is a group) added to hashtable, call
	 * {@link #addShapeWithChildren(java.lang.String, geditor.elements.GShape)} instead.
	 */
	public void addShape(String parentId, GShape shape) throws TreeInconsistencyException
	{
		addShape(parentId, shape, false);
	}

	/**
	 * Adds <code>shape</code> to the hashtable, adds shape to the group identified by <code>parentId</code>. If
	 * <code>shape</code> is a GGroup, it also adds all its children down to leaves to the hashtable.
	 */
	public void addShapeWithChildren(String parentId, GShape shape) throws TreeInconsistencyException
	{
		addShape(parentId, shape, true);
	}

	private void addShape(String parentId, GShape shape, boolean withChildren) throws TreeInconsistencyException
	{
		GShape parent = (GShape) shapes.get(parentId);

		if (parent == null)
		{
			throw new TreeInconsistencyException("Specified parent doesn't exist.");
		}

		GGroup parentGroup = null;

		if (parent.isLeaf())
		{
			throw new TreeInconsistencyException("The specified id was not of a GGroup object.");
		}
		else
		{
			parentGroup = (GGroup) parent;
		}

		parentGroup.addChild(shape);
		shape.setParent(parentGroup);

		if (withChildren)
		{
			addToShapes(shape);
		}
		else
		{
			shapes.put(shape.getId(), shape);
		}
	}

	private void addToShapes(GShape shape)
	{
		shapes.put(shape.getId(), shape);

		if (shape.isLeaf())
		{
			return;
		}
		else
		{
			GGroup shapeGroup = (GGroup) shape;
			for (int i = 0; i < shapeGroup.getNrChildren(); i++)
			{
				addToShapes(shapeGroup.childAt(i));
			}
		}
	}

	/**
	 * Deletes the shape identified by <code>id</code> from the hashtable, removes it from its parent and adds all
	 * its children as children of its parent. This is virtually an ungroup. Returns the deleted shape.
	 */
	public GShape deleteShape(String id)
	{
		return deleteShape(id, false);
	}

	/**
	 * Deletes the shape identified by <code>id</code> and all its children down to the leaves from the hashtable
	 * and removes it from its parent. Returns the deleted shape.
	 */
	public GShape deleteShapeWithChildren(String id)
	{
		return deleteShape(id, true);
	}

	private GShape deleteShape(String id, boolean withChildren)
	{
		GShape shape = (GShape) shapes.get(id);

		if (shape == null)
		{
			return null;
		}

		shape.getParent().deleteChild(shape);

		if (withChildren)
		{
			deleteFromShapes(id);
		}
		else
		{
			shapes.remove(id);

			if (!shape.isLeaf())
			{
				GGroup shapeGroup = (GGroup) shape;
				for (int i = 0; i < shapeGroup.getNrChildren(); i++)
				{
					GShape child = shapeGroup.childAt(i);

					shape.getParent().addChild(child);
					child.setParent(shape.getParent());
				}
			}
		}

		shape.setParent(null);

		return shape;
	}

	private void deleteFromShapes(String id)
	{
		GShape shape = (GShape) shapes.remove(id);

		if (shape.isLeaf())
		{
			return;
		}
		else
		{
			GGroup shapeGroup = (GGroup) shape;
			for (int i = 0; i < shapeGroup.getNrChildren(); i++)
			{
				deleteFromShapes(shapeGroup.childAt(i).getId());
			}
		}
	}

	/**
	 * Returns the shape identified by <code>id</code> or null if no such shape exists in the hashtable.
	 */
	public GShape getShape(String id)
	{
		return (GShape) shapes.get(id);
	}

	public Object clone()
	{
		GRootGroup clone = (GRootGroup) super.clone();

		clone.shapes = new Hashtable(shapes.size());

		clone.addToShapes(clone);

		return clone;
	}

	public void removeEmptyGroups()
	{
		removeEmptyGroups(this);
	}

	private void removeEmptyGroups(GGroup gr)
	{
		boolean bb = true;
		while (bb)
		{
			bb = false;
			for (int i = 0; i < gr.getNrChildren(); i++)
			{
				if (gr.childAt(i) instanceof GGroup)
				{
					removeEmptyGroups((GGroup)gr.childAt(i));
					if (((GGroup)gr.childAt(i)).getNrChildren() == 0)
					{
						deleteShape(gr.childAt(i).getId());
						bb = true;
						break;
					}
				}
			}
		}
	}
}