/**
 * A Singleton class used throughout the application for debugging purposes.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jun 2, 2004
 */
package utils;

public class Debugger
{
	/**
	 * Returns true if debugging is turned on and false if it is turned off.
	 */
	public boolean isDebugging()
	{
		return debugging;
	}

	/**
	 * If <code>debugging</code> is set to false, debugging is turned off and no debugging messages will
	 * be printed. If <code>debugging</code> is set to true, debugging is turned on and all debugging
	 * messages will be printed.
	 */
	public void setDebugging(boolean debugging)
	{
		this.debugging = debugging;
	}

	/**
	 * Posts a debugging message. If debugging is turned on, the message will be printed to the console.
	 * @param message the debug message
	 */
	public void postMessage(String message)
	{
		if (debugging)
		{
			System.out.println(message);
		}
	}

	public static Debugger getInstance()
	{
		return ourInstance;
	}

	private Debugger()
	{
		debugging = false;
	}

	private static Debugger ourInstance = new Debugger();

	private boolean debugging;
}
