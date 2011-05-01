package client.gui;

import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

/**
 * This class provides a graphical component based on a JTextField which allows the user to either edit the text field
 * as normal or select a value from a popup selection list which changes as he edits the text field.
 * <br /><br />
 * The popup disappears when the following keys are pressed: Enter, Esc.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 15, 2003
 */
public class SelectionList extends JTextField
{
	/**
	 * Build a SelectionList object with one of the two behaviours:
	 * <br />
	 * <ol>
	 * <li>
	 * DEFAULT_BEHAVIOUR - you supply an initial list of values and the component displays a proper sublist of this
	 * list automatically to the user. See descripion of {@link #setMatchingStyle setMatchingStyle} to see how this
	 * sublist is computed
	 * </li>
	 * <li>
	 * CUSTOM_BEHAVIOR - every time the user edits the text box, the
	 * {@link SelectionListListener#getListFor getListFor} method is called. You simply implement this method and
	 * return the list to be displayed by the component
	 * </li>
	 * </ol>
	 * <br />
	 *
	 * @param behaviour the behaviour to use
	 * @param allValues the list of values to use when behaviour = DEFAULT_BEHAVIOUR. Otherwise, this parameter is
	 *      ignored
	 * @param listener the SelectionListListener implementation to use
	 */
	public SelectionList(int behaviour, String[] allValues, SelectionListListener listener)
	{
		super();

		if ((behaviour != DEFAULT_BEHAVIOUR) && (behaviour != CUSTOM_BEHAVIOUR))
		{
			this.behaviour = DEFAULT_BEHAVIOUR;
		}
		else
		{
			this.behaviour = behaviour;
		}

		this.listener = listener;
		if (this.listener == null)
		{
			this.listener = new SelectionListAdapter();
		}
		if (this.behaviour == DEFAULT_BEHAVIOUR)
		{
			this.listener = new DefaultSelectionListListener(this.listener, allValues);
			((DefaultSelectionListListener) this.listener).setMatchingStyle(BEGIN_WITH_STYLE);
		}

		popup = null;
		lastText = "";
		lastRowCount = -1;

		currentList = new JList();
		currentList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		currentList.setBackground(new Color(235, 244, 254));
		currentList.setForeground(Color.BLACK);
		currentList.setSelectionForeground(Color.WHITE);
		currentList.setSelectionBackground(new Color(0, 82, 164));
		currentList.setFont(currentList.getFont().deriveFont(Font.BOLD));
		currentList.setFocusable(false);
		currentList.addListSelectionListener(new SelectionListListSelectionListener());

		listScrollPane = new JScrollPane();

		addKeyListener(new SelectionListKeyListener());
		addFocusListener(new SelectionListFocusListener());
		addComponentListener(new SelectionListComponentListener());
		addHierarchyBoundsListener(new SelectionListHierarchyBoundsListener());
	}

	/**
	 * By default, when the set behaviour is the default behaviour, the component creates the sublist to display by
	 * choosing all entries from the full list that start with the edited text. You can change this behaviour by
	 * using this method in order to set the behaviour to another of the predefined behaviours. The options are:
	 * <br />
	 * <list>
	 * <li>
	 * BEGIN_WITH_STYLE - matching is done by choosing those values from the initial full list
	 * (provided in the constructor) that begin with the edited text
	 * </li>
	 * <li>
	 * CONTAINS_STYLE - matching is done by choosing those values from the initial full list
	 * (provided in the constructor) that contain the edited text anywhere in the string
	 * </li>
	 * <li>
	 * EXTENDED_CONTAINS_STYLE - matching is done by choosing those values from the initial full list
	 * (provided in the constructor) that contain the edited text anywhere in the string, possibly with other
	 * charaters in between. This means that, for example, if the edited text is "his", this will match all of the
	 * following: "his", "hairs", "helicopters", "archbishop".
	 * </li>
	 * </list>
	 * <br />
	 * The method has no effect if the current behaviour is CUSTOM_BEHAVIOUR.
	 */
	public void setMatchingStyle(int matchingStyle)
	{
		if (behaviour == DEFAULT_BEHAVIOUR)
		{
			((DefaultSelectionListListener) this.listener).setMatchingStyle(matchingStyle);
		}
	}

	/**
	 * Use this predefined constant to specify that the selection list should display the default list to the user
	 * as the editing goes on.
	 */
	public final static int DEFAULT_BEHAVIOUR = 0;

	/**
	 * Use this predefined constant to specify that the selection list should call the
	 * {@link SelectionListListener#getListFor getListFor} method and allow you to choose the list to display to
	 * the user as the editing goes on.
	 */
	public final static int CUSTOM_BEHAVIOUR = 1;

	/**
	 * Predefined constant that specifies that matching is done by choosing those values from the initial full list
	 * (provided in the constructor) that begin with the edited text.
	 */
	public final static int BEGIN_WITH_STYLE = 0;

	/**
	 * Predefined constant that specifies that matching is done by choosing those values from the initial full list
	 * (provided in the constructor) that contain the edited text anywhere in the string.
	 */
	public final static int CONTAINS_STYLE = 1;

	/**
	 * Predefined constant that specifies that matching is done by choosing those values from the initial full list
	 * (provided in the constructor) that contain the edited text anywhere in the string, possibly with other
	 * charaters in between. This means that, for example, if the edited text is "his", this will match all of the
	 * following: "his", "hairs", "helicopters", "archbishop".
	 */
	public final static int EXTENDED_CONTAINS_STYLE =2;

	/**
	 * The behaviour to use.
	 */
	private int behaviour;

	/**
	 * Holds the current list displayed to the user.
	 */
	private JList currentList;

	/**
	 * The scroll pane used to display the list.
	 */
	JScrollPane listScrollPane;

	/**
	 * The listener used for callbacks.
	 */
	private SelectionListListener listener;

	/**
	 * The popup used to display the selection list.
	 */
	private Popup popup;

	/**
	 * The last known value from this text field.
	 */
	private String lastText;

	/**
	 * The last know row count of the list shown in the popup.
	 */
	private int lastRowCount;

	class SelectionListKeyListener extends KeyAdapter
	{
		private boolean actionKey(KeyEvent e)
		{
			if (popup == null)
			{
				return false;
			}

			int keyCode = e.getKeyCode();

			if (keyCode == KeyEvent.VK_DOWN)
			{
				int newIndex = currentList.getSelectedIndex() + 1;
				if (newIndex == currentList.getModel().getSize())
				{
					newIndex = 0;
				}

				currentList.setSelectedIndex(newIndex);
				moveViewport();
			}
			else if (keyCode == KeyEvent.VK_UP)
			{
				int newIndex = currentList.getSelectedIndex() - 1;
				if (newIndex == -1)
				{
					newIndex = currentList.getModel().getSize() - 1;
				}

				currentList.setSelectedIndex(newIndex);
				moveViewport();
			}
			else if (keyCode == KeyEvent.VK_ENTER)
			{
				listener.entrySelected(new SelectionListEvent(currentList.getSelectedIndex(),
					currentList.getSelectedIndex(), (String) currentList.getSelectedValue(),
					(String) currentList.getSelectedValue()));

				popup.hide();
				popup = null;
			}
			else if (keyCode == KeyEvent.VK_ESCAPE)
			{
				popup.hide();
				popup = null;
			}
			else
			{
				return false;
			}

			return true;
		}

		public void keyReleased(KeyEvent e)
		{
			if (actionKey(e))
			{
				return;
			}

/*
			// this was the old version, before introducing the lastText field
			int modMask = e.getModifiersEx();
			int mask = KeyEvent.CTRL_DOWN_MASK |
				KeyEvent.ALT_DOWN_MASK |
				KeyEvent.ALT_GRAPH_DOWN_MASK |
			        KeyEvent.META_DOWN_MASK;

			// if either of the Ctrl, Alt, AltGr or Meta keys is pressed, ignore the event
			if ((modMask & mask) != 0)
			{
				return;
			}

			// if a control key has been hit, ignore the event
			if ((Character.isISOControl(e.getKeyChar())) &&
				(e.getKeyChar() != KeyEvent.VK_BACK_SPACE) &&
				(e.getKeyChar() != KeyEvent.VK_DELETE))
			{
				return;
			}

			// if a action key has been hit (except Enter, Esc, down, up which are handled in the
			// actionKey method), ignore the event
			if (e.isActionKey())
			{
				return;
			}
*/

			if (lastText.equals(SelectionList.this.getText()))
			{
				return;
			}
			else
			{
				lastText = SelectionList.this.getText();
			}

			String oldValue = null;
			if (popup != null)
			{
				// store old selected value so that if it is still present in the new list, we can
				// set it the new list as the selected index to begin with
				oldValue = (String) currentList.getSelectedValue();
			}

			// obtain the list to display in the new popup
			String text = getText();
			String[] newList;
			if (text.equals(""))
			{
				if (popup != null)
				{
					popup.hide();
					popup = null;
				}
				return;
			}
			else
			{
				newList = listener.getListFor(text);
			}

			// don't show a popup if the list is empty
			if ((newList == null) || (newList.length == 0))
			{
				if (popup != null)
				{
					popup.hide();
					popup = null;
				}
				return;
			}

			currentList.setListData(newList);
			int newRowCount = Math.min(newList.length, 8);
			currentList.setVisibleRowCount(newRowCount);
			boolean needsNewPopup = false;
			if ((lastRowCount == -1) || (lastRowCount != newRowCount))
			{
				needsNewPopup = true;
				lastRowCount = newRowCount;
			}

			// try to set the selected index so that it points to the old selected value
			int selectedIndex = 0;
			if (oldValue != null)
			{
				for (int i = 0; i < newList.length; i++)
				{
					if (oldValue.equals(newList[i]))
					{
						selectedIndex = i;
						i = newList.length;
					}
				}
			}
			currentList.setSelectedIndex(selectedIndex);

			// update the list's view
			listScrollPane.setViewportView(currentList);
			moveViewport();

			if ((popup == null) || (needsNewPopup == true))
			{
				if (popup != null)
				{
					popup.hide();
				}
				Point loc = SelectionList.this.getLocationOnScreen();
				popup = PopupFactory.getSharedInstance().getPopup(SelectionList.this,
					listScrollPane, (int) (loc.getX() + SelectionList.this.getWidth()),
					(int) loc.getY());
				popup.show();
			}
		}

		/**
		 * Moves the scroll pane's view port so that the element at the selected index is centered in the
		 * view port if possible.
		 */
		private void moveViewport()
		{
			int currentCell = currentList.getSelectedIndex();
			int nrVisibleCells = currentList.getVisibleRowCount();
			int nrCellsAbove, nrCellsBelow;

			if (nrVisibleCells % 2 == 0)
			{
				nrCellsAbove = nrVisibleCells / 2 - 1;
				nrCellsBelow = nrVisibleCells / 2;
			}
			else
			{
				nrCellsAbove = nrCellsBelow = (nrVisibleCells - 1) / 2;
			}

			int from = currentCell - nrCellsAbove;
			int to = currentCell + nrCellsBelow;

			if (from < 0)
			{
				from = 0;
				to = nrVisibleCells - 1;
			}
			else if (to >= currentList.getModel().getSize())
			{
				from = currentList.getModel().getSize() - nrVisibleCells;
				to = currentList.getModel().getSize() - 1;
			}

			Rectangle rec = currentList.getCellBounds(from, to);
			listScrollPane.getViewport().setViewPosition(new Point((int) rec.getX(), (int) rec.getY()));
		}
	}

	class SelectionListFocusListener extends FocusAdapter
	{
		public void focusLost(FocusEvent e)
		{
			if (popup != null)
			{
				popup.hide();
				popup = null;
			}
		}
	}

	class SelectionListComponentListener extends ComponentAdapter
	{
		public void componentResized(ComponentEvent e)
		{
			if (popup != null)
			{
				popup.hide();
			}

			popup = null;
		}

		public void componentMoved(ComponentEvent e)
		{
			if (popup != null)
			{
				popup.hide();
			}

			popup = null;
		}
	}

	class SelectionListHierarchyBoundsListener extends HierarchyBoundsAdapter
	{
		public void ancestorMoved(HierarchyEvent e)
		{
			if (popup != null)
			{
				popup.hide();
			}

			popup = null;
		}
	}

	class SelectionListListSelectionListener implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent e)
		{
			int oldIndex = e.getFirstIndex();
			int newIndex = currentList.getSelectedIndex();
			if (oldIndex == newIndex)
			{
				oldIndex = e.getLastIndex();
			}

			if ((oldIndex < 0) || (newIndex < 0) || (oldIndex >= currentList.getModel().getSize()))
			{
				return;
			}

			String oldValue = (String) currentList.getModel().getElementAt(oldIndex);
			String newValue = (String) currentList.getSelectedValue();




			listener.selectionIndexChanged(new SelectionListEvent(oldIndex, newIndex, oldValue, newValue));
		}
	}

	/**
	 * This class represents an implementation of the SelectionListListener interface which handles the default
	 * policy of this component.
	 */
	class DefaultSelectionListListener implements SelectionListListener
	{
		public DefaultSelectionListListener(SelectionListListener delegateListener, String[] allValues)
		{
			this.delegateListener = delegateListener;
			this.fullList = allValues;
			this.matchingStyle = SelectionList.BEGIN_WITH_STYLE;
		}

		public void setMatchingStyle(int matchingStyle)
		{
			if ((matchingStyle == BEGIN_WITH_STYLE) || (matchingStyle == CONTAINS_STYLE) ||
				(matchingStyle == EXTENDED_CONTAINS_STYLE))
			{
				this.matchingStyle = matchingStyle;
			}
		}

		public void selectionIndexChanged(SelectionListEvent selListEvent)
		{
			delegateListener.selectionIndexChanged(selListEvent);
		}

		public void entrySelected(SelectionListEvent selListEvent)
		{
			delegateListener.entrySelected(selListEvent);
		}

		public String[] getListFor(String text)
		{
			ArrayList result;
			try
			{
				result = new ArrayList(Math.max(fullList.length / text.length(), 5));
			}
			catch (ArithmeticException e)
			{
				result = new ArrayList(5);
			}

			switch (matchingStyle)
			{
				case SelectionList.BEGIN_WITH_STYLE:
					for (int i = 0; i < fullList.length; i++)
					{
						if (fullList[i].startsWith(text))
						{
							result.add(fullList[i]);
						}
					}
					break;
				case SelectionList.CONTAINS_STYLE:
					for (int i = 0; i < fullList.length; i++)
					{
						if (fullList[i].indexOf(text) != -1)
						{
							result.add(fullList[i]);
						}
					}
					break;
				case SelectionList.EXTENDED_CONTAINS_STYLE:
					break;
			}

			String[] temp = new String[result.size()];
			result.toArray(temp);

			return temp;
		}

		/**
		 * The listener used for sending selectionIndexChanged and entrySelected events.
		 */
		private SelectionListListener delegateListener;

		/**
		 * Defines the way matching is done for the default behaviour.
		 */
		private int matchingStyle;

		/**
		 * The list of values to use when behaviour = DEFAULT_BEHAVIOUR.
		 */
		private String[] fullList;
	}
}