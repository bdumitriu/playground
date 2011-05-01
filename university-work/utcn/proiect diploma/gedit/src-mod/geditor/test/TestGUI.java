package geditor.test;

import geditor.gui.UserFrame;
import geditor.gui.MainFrame;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Mar 15, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class TestGUI
{
	public static void main(String[] args)
	{
		MainFrame.getInstance().initDrawboard();
		MainFrame.getInstance().show();
	}
}
