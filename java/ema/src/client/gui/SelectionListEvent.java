package client.gui;

/**
 * This class encapsulates all data referring to a SelectionList event.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 21, 2003
 */
public class SelectionListEvent
{
	public SelectionListEvent(int oldIndex, int newIndex, String oldValue, String newValue)
	{
		this.oldIndex = oldIndex;
		this.newIndex = newIndex;
		this.oldValue = oldValue;
		this.newValue = newValue;
	}

	/**
	 * Returns the list index that was selected before the event. Returns -1 the first time an entry is chosen.
	 */
	public int getOldIndex()
	{
		return oldIndex;
	}

	/**
	 * Returns the currently selected list index.
	 */
	public int getNewIndex()
	{
		return newIndex;
	}

	/**
	 * Returns the list value that was selected before the event. Returns null the first time an entry is chosen.
	 */
	public String getOldValue()
	{
		return oldValue;
	}

	/**
	 * Returns the currently selected list value.
	 */
	public String getNewValue()
	{
		return newValue;
	}

	/**
	 * The list index that was selected before the event.
	 */
	private int oldIndex;

	/**
	 * The currently selected list index.
	 */
	private int newIndex;

	/**
	 * The list value that was selected before the event.
	 */
	private String oldValue;

	/**
	 * The currently selected list value.
	 */
	private String newValue;
}
