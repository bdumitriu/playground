package client.gui.iv;

import javax.swing.*;
import javax.swing.text.JTextComponent;

/**
 * An javax.swing.InputVerifier implementation that checks that the text in the input field is a valid Integer.
 * Additionally, it also offers the possibility of setting an inferior and/or superior limit of the value to be checked.
 * By default, the empty string is not allowed. If you want to allow it, use the
 * {@link #allowEmptyString allowEmptyString} method.
 * <br /><br />
 * <b>Note:</b> This component only works with JTextComponent's. For any other type of component it has no effect (i.e.
 * the {@link #verify verify} method returns true all the time).
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 25, 2003
 */
public class IntegerInputVerifier extends InputVerifier
{
	/**
	 * Creates a new IntegerInputVerifier with no inferior/superior limit.
	 */
	public IntegerInputVerifier()
	{
		minSet = maxSet = false;
		allowEmptyString = false;
	}

	/**
	 * Creates a new IntegerInputVerifier with both inferior and superior limits. To create a IntegerInputVerifier
	 * that only uses an inferior/superior limit, create the initial IntegerInputVerifier with the no parameter
	 * constructor and set the limit with the appropriate method.
	 *
	 * @param min the inferior limit (the value of <code>min</code> is also allowed)
	 * @param max the superior limit (the value of <code>max</code> is also allowed)
	 */
	public IntegerInputVerifier(int min, int max)
	{
		this.min = min;
		this.max = max;
		minSet = maxSet = true;
		allowEmptyString = false;
	}

	/**
	 * Tell the verifier whether to allow the empty string or not.
	 *
	 * @param allowEmptyString if true, allow it; if false, disallow it
	 */
	public void allowEmptyString(boolean allowEmptyString)
	{
		this.allowEmptyString = allowEmptyString;
	}

	/**
	 * Sets the inferior limit to <code>min</code>.
	 *
	 * @param min the new inferior limit (the value of <code>min</code> is also allowed)
	 */
	public void setMin(int min)
	{
		this.min = min;
		minSet = true;
	}

	/**
	 * Sets the superior limit to <code>max</code>.
	 *
	 * @param max the new superior limit (the value of <code>max</code> is also allowed)
	 */
	public void setMax(int max)
	{
		this.max = max;
		maxSet = true;
	}

	public int getMin()
	{
		return min;
	}

	public int getMax()
	{
		return max;
	}

	/**
	 * Determines the verifier no longer to impose the inferior limit.
	 */
	public void unsetMin()
	{
		minSet = false;
	}

	/**
	 * Determines the verifier no longer to impose the superior limit.
	 */
	public void unsetMax()
	{
		maxSet = false;
	}

	public boolean verify(JComponent input)
	{
		if (!(input instanceof JTextComponent))
		{
			return true;
		}

		String text = ((JTextComponent) input).getText();

		try
		{
			if ((allowEmptyString == true) && (text.equals("")))
			{
				return true;
			}

			Integer nr = new Integer(text);
			int intNr = nr.intValue();

			if ((minSet == true) && (intNr < min))
			{
				return false;
			}
			if ((maxSet == true) && (intNr > max))
			{
				return false;
			}

			return true;
		}
		catch (NumberFormatException e)
		{
			return false;
		}
	}

	private int min;
	private int max;
	private boolean minSet;
	private boolean maxSet;
	private boolean allowEmptyString;
}
