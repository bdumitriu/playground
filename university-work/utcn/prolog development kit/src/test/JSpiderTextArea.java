package test;

import javax.swing.*;
import javax.swing.event.DocumentListener;
import javax.swing.event.DocumentEvent;
import javax.swing.border.Border;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Feb 15, 2003
 * Time: 3:41:08 PM
 * To change this template use Options | File Templates.
 */
public class JSpiderTextArea extends JComponent
{
	private static String CENTER = "center";
	private static String RIGHT = "right";
	private static String LEFT = "left";
	private static String BOTTOM = "bottom";

	public JSpiderTextArea()
	{
		setLayout(new ScrollLayout());
	}

	class ScrollLayout implements LayoutManager
	{
		private Component center;
		private Component left;
		private Component right;
		private Component bottom;

		public void addLayoutComponent(String name, Component comp)
		{
			if (name.equals(CENTER))
				center = comp;
			else if (name.equals(RIGHT))
				right = comp;
			else if (name.equals(LEFT))
				left = comp;
			else if (name.equals(BOTTOM))
				bottom = comp;
		}

		public void removeLayoutComponent(Component comp)
		{
			if (center == comp)
				center = null;
			else if (right == comp)
				right = null;
			else if (left == comp)
				left = null;
			else if (bottom == comp)
				bottom = null;
		}

		public Dimension preferredLayoutSize(Container parent)
		{
			Dimension dim = new Dimension();
			Border border = getBorder();
			Insets insets;

			if (border == null)
				insets = new Insets(0, 0, 0, 0);
			else
			{
				insets = getBorder().getBorderInsets(
					JSpiderTextArea.this);
			}

			dim.width = insets.left + insets.right;
			dim.height = insets.top + insets.bottom;

			Dimension leftPref = left.getPreferredSize();
			Dimension centerPref = center.getPreferredSize();
			Dimension rightPref = right.getPreferredSize();
			Dimension bottomPref = bottom.getPreferredSize();

			dim.width += leftPref.width + centerPref.width + rightPref.width;
			dim.height += centerPref.height + bottomPref.height;

			return dim;
		}

		public Dimension minimumLayoutSize(Container parent)
		{
			Dimension dim = new Dimension();
			Border border = getBorder();
			Insets insets;

			if (border == null)
				insets = new Insets(0, 0, 0, 0);
			else
			{
				insets = getBorder().getBorderInsets(
					JSpiderTextArea.this);
			}

			dim.width = insets.left + insets.right;
			dim.height = insets.top + insets.bottom;

			Dimension leftPref = left.getMinimumSize();
			Dimension centerPref = center.getMinimumSize();
			Dimension rightPref = right.getMinimumSize();
			Dimension bottomPref = bottom.getMinimumSize();

			dim.width += leftPref.width + centerPref.width + rightPref.width;
			dim.height += centerPref.height + bottomPref.height;

			return dim;
		}

		public void layoutContainer(Container parent)
		{
			Dimension size = parent.getSize();
			Border border = getBorder();
			Insets insets;

			if (border == null)
				insets = new Insets(0, 0, 0, 0);
			else
			{
				insets = getBorder().getBorderInsets(
					JSpiderTextArea.this);
			}

			int itop = insets.top;
			int ileft = insets.left;
			int ibottom = insets.bottom;
			int iright = insets.right;

			int rightWidth = right.getPreferredSize().width;
			int leftWidth = left.getPreferredSize().width;
			int bottomHeight = bottom.getPreferredSize().height;
			int centerWidth = Math.max(0, size.width - leftWidth
				- rightWidth - ileft - iright);
			int centerHeight = Math.max(0, size.height
				- bottomHeight - itop - ibottom);

			left.setBounds(
				ileft,
				itop,
				leftWidth,
				centerHeight);

			center.setBounds(
				ileft + leftWidth,
				itop,
				centerWidth,
				centerHeight);

			right.setBounds(
				ileft + leftWidth + centerWidth,
				itop,
				rightWidth,
				centerHeight);

			bottom.setBounds(
				ileft,
				itop + centerHeight,
				Math.max(0, size.width - rightWidth - ileft - iright),
				bottomHeight);
		}
	}
}