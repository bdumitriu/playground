package gui;

import data.EditorUser;

/**
 * This interface defines the operations that UserListGui will call externally.
 * <br /><br />
 * Date: Mar 9, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public interface UserListFunctions
{
	/**
	 * Synchronizes this document with the one at machine identified by <code>ip</code>.
	 *
	 * @param ip the ip of the machine to synchronize with
	 * @param asMaster if true, this host will be the master  
	 */
	public void synchronize(String ip, boolean asMaster);

	/**
	 * Updates the text as an effect of an EditorUser color change. This method should redraw the text belonging to
	 * the user using the new color.
	 */
	public void updateText(EditorUser editorUser);
}
