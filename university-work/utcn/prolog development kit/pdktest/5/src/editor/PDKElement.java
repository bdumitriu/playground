package editor;

import javax.swing.text.Element;

/**
 * This interface simply adds an additional method to the ones provided by
 * the Element interface.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 * 
 * Date: Feb 25, 2003
 */
public interface PDKElement extends Element
{
	/**
	 * Fetches the type of this element.
	 *
	 * @return the type of this element.
	 */
	public int getType();
}
