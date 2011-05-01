package client.gui;

/**
 * The interface to implement in order to work with a SelectionList.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 21, 2003
 */
public interface SelectionListListener
{
	/**
	 * This method is called when the selection in the list changes.
	 *
	 * @param selListEvent the event containing the necessary information to process the event
	 */
	public void selectionIndexChanged(SelectionListEvent selListEvent);

	/**
	 * This method is called when the user chooses an entry by clicking it with the mouse or hitting the Enter key.
	 * In this case, newIndex will always be equal to oldIndex and newValue will always be equal to oldValue. 
	 *
	 * @param selListEvent the event containing the necessary information to process the event
	 */
	public void entrySelected(SelectionListEvent selListEvent);

	/**
	 * This method is only used when you specify in the constructor of the SelectionList that you wish to provide
	 * the list of String's yourself everytime the edited text is changed by the user.
	 *
	 * This method is called whenever the user edits the text box used in the SelectionList. The list of String's
	 * returned by the method will be used instead of the default to display to the user.
	 *
	 * @return a list of String's to display to the user
	 */
	public String[] getListFor(String text);
}
