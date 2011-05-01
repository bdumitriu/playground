package geditor.elements;

import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 10, 2003
 * Time: 10:53:51 AM
 */
public class GSelector
{
	public final static int SELECTED_RECT_DIM = 5;
	//types
	public final static int OBJECT = 1;
	public final static int GROUP = 2;

	public static void select(GShape obj, Graphics2D g2, int type)
	{
		switch (type)
		{
			case OBJECT:
				selectObject(obj, g2);
				break;
			case GROUP:
				selectGroup(obj, g2);
				break;
		}
	}

	/**
	 * ***************** select group ******************************
	 */
	private static void selectGroup(GShape obj, Graphics g)
	{
		Graphics2D g2 = (Graphics2D) g;

		int x1 = (int)obj.getMinPoint().x;
		int x2 = (int)obj.getMaxPoint().x;
		int y1 = (int)obj.getMinPoint().y;
		int y2 = (int)obj.getMaxPoint().y;

		g2.setColor(Color.black);

		g2.drawRect(x1 - (SELECTED_RECT_DIM / 2), y1 - (SELECTED_RECT_DIM / 2), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		g2.drawRect(x1 - (SELECTED_RECT_DIM / 2), y2 - (SELECTED_RECT_DIM / 2), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		g2.drawRect(x2 - (SELECTED_RECT_DIM / 2), y1 - (SELECTED_RECT_DIM / 2), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
		g2.drawRect(x2 - (SELECTED_RECT_DIM / 2), y2 - (SELECTED_RECT_DIM / 2), SELECTED_RECT_DIM, SELECTED_RECT_DIM);
	}

	/**
	 * ***************** select object ******************************
	 */
	private static void selectObject(GShape obj, Graphics2D g2)
	{
		obj.selectObject(g2);
	}
}
