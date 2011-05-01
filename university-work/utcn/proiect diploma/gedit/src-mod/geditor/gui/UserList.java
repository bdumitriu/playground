package geditor.gui;

import geditor.users.EditorUser;

import javax.swing.*;
import java.util.ArrayList;

/**
 * Manages a list of {@link EditorUser EditorUser}'s.
 * <br /><br />
 * Date: Mar 8, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class UserList extends AbstractListModel
{
	public UserList()
	{
		list = new ArrayList();
	}

	/**
	 * Adds <code>editorUser</code> to the list.
	 *
	 * @param editorUser the EditorUser to add
	 * @return true if <code>editorUser</code> was added successfully, false if an EditorUser with the same ip
	 *	already exists in the list
	 */
	public boolean addEditorUser(EditorUser editorUser)
	{
		if (getEditorUserByIp(editorUser.getIp()) != null)
		{
			return false;
		}
		else
		{
			list.add(editorUser);
			fireIntervalAdded(this, list.size() - 1, list.size() - 1);
			return true;
		}
	}

	/**
	 * Removes <code>editorUser</code> from the list. Comparison is done using the equals() method, not ==.
	 *
	 * @param editorUser the EditorUser to remove
	 * @return true if object was in the list (and was, consequently, remove), false otherwise
	 */
	public boolean removeEditorUser(EditorUser editorUser)
	{
		boolean retValue = list.remove(editorUser);

		if (retValue == true)
		{
			fireIntervalRemoved(this, list.size(), list.size());
		}

		return retValue;
	}

	/**
	 * Removes element <code>index</code> from the list.
	 *
	 * @param index the index of the element to remove
	 * @return the removed element or null if <code>index</code> was out of bounds
	 */
	public EditorUser removeEditorUser(int index)
	{
		if (index < 0 || index >= list.size())
		{
			return null;
		}

		EditorUser editorUser = (EditorUser) list.remove(index);

		fireIntervalRemoved(this, index, index);

		return editorUser;
	}

	/**
	 * Replaces the <code>index</code>th entry in the list with <code>editorUser</code>.
	 *
	 * @param index the number of the entry to replace
	 * @param editorUser the new value to put into the list
	 * @return true if replacement successful, false otherwise (i.e. if <code>index</code> was out of bounds
	 */
	public boolean setEditorUser(int index, EditorUser editorUser)
	{
		if (index < 0 || index >= list.size())
		{
			return false;
		}

		list.set(index, editorUser);

		fireContentsChanged(this, index, index);

		return true;
	}

	/**
	 * Returns the EditorUser on position <code>index</code>.
	 *
	 * @param index the position of the EditorUser you want
	 * @return the EditorUser on positio <code>index</code> or null if <code>index</code> was out of bounds
	 */
	public EditorUser getEditorUser(int index)
	{
		if (index < 0 || index >= list.size())
		{
			return null;
		}

		return (EditorUser) list.get(index);
	}

	/**
	 * Returns the EditorUser (if one exists) with ip equal to <code>ip</code>.
	 *
	 * @param ip the ip to compare the ip of each element in the list to
	 * @return the EditorUser (if one exists) with ip equal to <code>ip</code> or null if no such EditorUser exists
	 *	in the list
	 */
	public EditorUser getEditorUserByIp(String ip)
	{
		for (int i = 0; i < list.size(); i++)
		{
			EditorUser current = (EditorUser) list.get(i);

			if (current.getIp().equals(ip))
			{
				return current;
			}
		}

		return null;
	}

	/**
	 * Returns the EditorUser (if one exists) with username equal to <code>username</code>.
	 *
	 * @param username the username to compare the username of each element in the list to
	 * @return the EditorUser (if one exists) with username equal to <code>username</code> or null if no such
	 *	EditorUser exists in the list
	 */
	public EditorUser getEditorUserByName(String username)
	{
		for (int i = 0; i < list.size(); i++)
		{
			EditorUser current = (EditorUser) list.get(i);

			if (current.getUsername().equals(username))
			{
				return current;
			}
		}

		return null;
	}

	public int getSize()
	{
		return list.size();
	}

	public Object getElementAt(int index)
	{
		return list.get(index);
	}

	private ArrayList list;
}
