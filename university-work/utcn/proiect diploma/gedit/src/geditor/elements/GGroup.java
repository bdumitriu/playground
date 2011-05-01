package geditor.elements;

import geditor.engine.tree.ObjectProperties;

import java.awt.*;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collections;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Mar 16, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */


public class GGroup extends GShape
{
	ArrayList children;

	public GGroup()
	{
		super(-1);

		children = new ArrayList();
	}

	/**
	 * Adds <code>child</code> to this group.
	 */
	public synchronized void addChild(GShape child)
	{
		children.add(child);
	}

	/**
	 * Deletes <code>child</code> from this group. Returns true if child existed and false otherwise.
	 */
	public synchronized boolean deleteChild(GShape child)
	{
		return children.remove(child);
	}

	/**
	 * Returns the number of children of this group.
	 */
	public int getNrChildren()
	{
		return children.size();
	}

	/**
	 * Returns true if this group has no children and false otherwise.
	 * @return
	 */
	public boolean isEmpty()
	{
		return children.isEmpty();
	}

	/**
	 * Returns the child at position <code>index</code> or null if position is invalid.
	 */
	public GShape childAt(int index)
	{
		if (index < 0 || index >= children.size())
		{
			return null;
		}
		else
		{
			return (GShape) children.get(index);
		}
	}

	public GShape findChild(String id)
	{
		for (int i = 0; i < getNrChildren(); i++)
			if (childAt(i).id.equals(id)) return childAt(i);
		return null;
	}



	/**
	 * {@inheritDoc}
	 */
	public double getWidth()
	{
		return Math.abs(getMaxPoint().x - getMinPoint().x);
	}

	/**
	 * {@inheritDoc}
	 */
	public double getHeight()
	{
		return Math.abs(getMaxPoint().y - getMinPoint().y);
	}

	/**
	 * {@inheritDoc}
	 */
	public Point2DDouble getMaxPoint()
	{
		double maxPointX = -1;
		double maxPointY = -1;

		for (int i = 0; i < children.size(); i++)
		{
			GShape current = (GShape) children.get(i);

			Point2DDouble currentMP = current.getMaxPoint();

			if (maxPointX < currentMP.x)
			{
				maxPointX = currentMP.x;
			}

			if (maxPointY < currentMP.y)
			{
				maxPointY = currentMP.y;
			}
		}

		return new Point2DDouble(maxPointX, maxPointY);
	}

	/**
	 * {@inheritDoc}
	 */
	public Point2DDouble getMinPoint()
	{
		double minPointX = -1;
		double minPointY = -1;

		if (children.size() >= 1)
		{
			GShape current = (GShape) children.get(0);

			minPointX = current.getMinPoint().x;
			minPointY = current.getMinPoint().y;
		}

		for (int i = 1; i < children.size(); i++)
		{
			GShape current = (GShape) children.get(i);

			Point2DDouble currentMP = current.getMinPoint();

			if (minPointX > currentMP.x)
			{
				minPointX = currentMP.x;
			}

			if (minPointY > currentMP.y)
			{
				minPointY = currentMP.y;
			}
		}

		return new Point2DDouble(minPointX, minPointY);
	}

	/**
	 * Returns the minimum value of z from the z's of its children.
	 */
	public double getMinZ()
	{
		double minZ;
		if (children.isEmpty())
		{
			return 0;
		}
		else
		{
			minZ = ((GShape) children.get(0)).getZ();
		}

		for (int i = 0; i < children.size(); i++)
		{
			double currentZ = ((GShape) children.get(i)).getZ();
			if (minZ > currentZ)
			{
				minZ = currentZ;
			}
		}

		return minZ;
	}

	/**
	 * Returns the maximum value of z from the z's of its children.
	 */
	public double getMaxZ()
	{
		double maxZ;
		if (children.isEmpty())
		{
			return 0;
		}
		else
		{
			maxZ = ((GShape) children.get(0)).getZ();
		}

		for (int i = 0; i < children.size(); i++)
		{
			double currentZ = ((GShape) children.get(i)).getZ();
			if (maxZ < currentZ)
			{
				maxZ = currentZ;
			}
		}

		return maxZ;
	}


	/**
	 * {@inheritDoc}
	 */
	public void moveWith(double dx, double dy)
	{

		for (int i = 0; i < children.size(); i++)
		{

                childAt(i).moveWith(dx, dy);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public ObjectProperties getObjectProperties()
	{
		ObjectProperties op = new ObjectProperties(id, (int) getMinPoint().x, (int) getMinPoint().y,
			(int) getWidth(), (int) getHeight(), null, null, null);

		op.setGroup(true);

		return op;
	}

	public void draw(Graphics2D graphics)
	{
		Collections.sort(children);
		for (int i = 0; i < children.size(); i++)
		{
			GShape current = (GShape) children.get(i);
			current.draw(graphics);
		}
		if (selected) selectObject(graphics);
	}

	public boolean isLeaf()
	{
		return false;
	}

	public void scale(double refx, double refy, double tx, double ty)
	{
		for (int i = 0; i < getNrChildren(); i++)
		{
			childAt(i).scale(refx, refy, tx, ty);
		}
	}

	/**
	 * Looks for a child with the specified <code>id</code> and deletes it if it finds it. Returns the deleted child
	 * or null if no child with the specified <code>id</code> exists in this group.
	 */
	public synchronized GShape deleteChild(String id)
	{
		int index = -1;
		for (int i = 0; i < children.size(); i++)
		{
			GShape current = (GShape) children.get(i);

			if (current.getId().equals(id))
			{
				index = i;
				i = children.size();
			}
		}

		if (index != -1)
		{
			return (GShape) children.remove(index);
		}
		else
		{
			return null;
		}
	}

	
	public Object clone()
	{
		GGroup gGroup = (GGroup) super.clone();

		gGroup.children = (ArrayList) children.clone();

		for (int i = 0; i < children.size(); i++)
		{
			gGroup.children.set(i, ((GShape) children.get(i)).clone());
			((GShape) gGroup.children.get(i)).setParent(gGroup);
		}

		return gGroup;
	}

	public void setFgColor(Color fgColor)
	{
		super.setFgColor(fgColor);
		for (int i = 0; i < getNrChildren(); i++)
			childAt(i).setFgColor(fgColor);
	}

	public void setBgColor(Color bgColor)
	{
		super.setBgColor(bgColor);
		for (int i = 0; i < getNrChildren(); i++)
			childAt(i).setBgColor(bgColor);
	}

	public ArrayList getNodesByZ()
	{
		ArrayList result = new ArrayList();
		for (int i = 0; i < getNrChildren(); i++)
		{
			GShape child = childAt(i);
			if (child instanceof GGroup)
				result.addAll(((GGroup)child).getNodesByZ());
			else
			result.add(child);
		}
		return result;
	}

	public boolean insideObject(Point2DDouble p)
	{
		for (int i = 0; i < getNrChildren(); i++)
			if (childAt(i).insideObject(p)) return true;
		return false;
	}

	public String toString()
	{
		String result = "{ ";
		for (int i = 0; i < getNrChildren(); i++)
		{
			result += childAt(i) + " ";
			if (i != getNrChildren() - 1) result += ", ";
		}
		result += "}";
		return result;
	}




	public ArrayList getChildren()
	{
		return children;
	}


	public boolean existsId(String id)
	{
		if (super.existsId(id)) return true;

		for (int i = 0; i < getNrChildren(); i++)
			if (childAt(i).existsId(id)) return true;
		return false;
	}

	public Color getBgColor()
	{
		if (getNrChildren() == 0) return null;
		Color result = ((GShape)children.get(0)).getBgColor();
		if (result == null) return null;

		for (int i = 1; i < getNrChildren(); i++)
		{
			Color c = ((GShape)children.get(i)).getBgColor();
			if (!result.equals(c)) return null;
		}
		return result;
	}

	public Color getFgColor()
	{
		if (getNrChildren() == 0) return null;
		Color result = ((GShape)children.get(0)).getFgColor();
		if (result == null) return null;

		for (int i = 1; i < getNrChildren(); i++)
		{
			Color c = ((GShape)children.get(i)).getFgColor();
			if (!result.equals(c)) return null;
		}
		return result;
	}

	public void clearSelection()
	{
		super.clearSelection();
		for (int i = 0; i < getNrChildren(); i++)
			childAt(i).clearSelection();
	}

	public void selectAllGroups()
	{
		selected = true;
		for (int i = 0; i < getNrChildren(); i++)
			childAt(i).selectAllGroups();		
	}

	public ArrayList getLeafs()
	{
		ArrayList result = new ArrayList();
		for (int i = 0; i < getNrChildren(); i++)
			result.addAll(childAt(i).getLeafs());
		return result;		
	}

	public void clean()
	{
		super.clean();
		for (int i = 0; i < getNrChildren(); i++)
			childAt(i).clean();
	}

	public void deleteAllChildren()
	{
		children.clear();
	}

	public void clearVisitedFlag()
	{
		super.clearVisitedFlag();
		for (int i = 0; i < getNrChildren(); i++)
			childAt(i).clearVisitedFlag();

	}

	public boolean equals(Object obj)
	{
		if (!(obj instanceof GGroup)) return false;
		
		GGroup gr = (GGroup) obj;

		if (!gr.getId().equals(getId())) return false;
		if (gr.getNrChildren() != getNrChildren()) return false;

		Collections.sort(children);
		Collections.sort(gr.children);

		for (int i = 0; i < getNrChildren(); i++)
			if (!childAt(i).equals(gr.childAt(i))) return false;
		return true;
	}



}