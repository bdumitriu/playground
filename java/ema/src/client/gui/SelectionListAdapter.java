package client.gui;

/**
 * Provides a default implementation for the SelectionListListener interface. The methods provide no behaviour.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 21, 2003
 */
public class SelectionListAdapter implements SelectionListListener
{
	public void selectionIndexChanged(SelectionListEvent selListEvent)
	{}

	public void entrySelected(SelectionListEvent selListEvent)
	{}

	public String[] getListFor(String text)
	{
		return null;
	}
}
