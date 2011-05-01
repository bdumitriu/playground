package client.gui;

import javax.swing.*;

/**
 * This interface is the root of all GUI screens of the application. It defines the methods that need to be implemented
 * in order to provide the application with a single way for displaying all components.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 13, 2003
 */
public interface Display
{
	/**
	 * This method will be called each time this screen is displayed to the user. The screen should add all its
	 * components to the parent frame, set the frame's layout and, basically, do everything in order for it to be
	 * properly displayed.
	 * <br /><br />
	 * Don't forget to call <code>parent.validate()</code> once you finish setting everything up.
	 *
	 * @param parent the JFrame in which this screen will be displayed
	 */
	public void onShow(JFrame parent);

	/**
	 * Thsi method will be called each time this screen is hidden. Since the screen might never be displayed again
	 * throught the execution of the application, it is advisable that you save everything you need to in the body
	 * of this method. You should also clean up the frame by removing all the components you have added in
	 * {@link #onShow onShow}.
	 *
	 * @param parent the JFrame in which this screen is currently displayed
	 */
	public void onHide(JFrame parent);
}
