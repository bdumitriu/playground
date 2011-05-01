package geditor.engine.tree;

import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 11, 2003
 * Time: 11:44:47 AM
 */
public class ObjectProperties
{
	protected Color fgColor;
	protected Color bgColor;
	protected String id;
	protected int left;
	protected int top;
	protected int width;
	protected int height;
	protected String text;
	protected boolean isGroup;

	public ObjectProperties(String id, int left, int top, int width, int height, Color fgColor, Color bgColor, String text)
	{
		this.id = id;
		this.fgColor = fgColor;
		this.bgColor = bgColor;
		this.left = left;
		this.top = top;
		this.width = width;
		this.width = width;
		this.height = height;
		this.text = text;
		this.isGroup = false;
	}

	public ObjectProperties()
	{
		this.id = "";
		this.fgColor = null;
		this.bgColor = null;
		this.left = 0;
		this.top = 0;
		this.width = 0;
		this.width = 0;
		this.height = 0;
		this.text = null;
	}

	public Color getFgColor()
	{
		return fgColor;
	}

	public Color getBgColor()
	{
		return bgColor;
	}

	public String getId()
	{
		return id;
	}

	public int getLeft()
	{
		return left;
	}

	public int getTop()
	{
		return top;
	}

	public int getWidth()
	{
		return width;
	}

	public int getHeight()
	{
		return height;
	}

	public String getText()
	{
		return text;
	}

	public void setGroup(boolean isGroup)
	{
		this.isGroup = isGroup;
	}

	public boolean isGroup()
	{
		return isGroup;
	}
}

