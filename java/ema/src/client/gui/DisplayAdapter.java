package client.gui;

import javax.swing.*;

/**
 * This class provides a partial implementation of the Display interface, making it easier for simple screens to define
 * their behaviour.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 13, 2003
 */
public abstract class DisplayAdapter implements Display
{
	/**
	 * Simply clears the frame of all its components.
	 *
	 * @param parent the JFrame in which this screen will be displayed
	 */
	public void onHide(JFrame parent)
	{
		parent.getContentPane().removeAll();
	}
}
