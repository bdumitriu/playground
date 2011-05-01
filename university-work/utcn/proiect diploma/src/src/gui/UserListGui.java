package gui;

import data.EditorUser;

import javax.swing.*;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;

import gui.diag.AddEditorUserDialog;

/**
 * This is a graphical component used for displaying a list of users.
 * <br /><br />
 * Date: Mar 8, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class UserListGui extends JPanel implements ActionListener, FocusListener
{
	/**
	 * @param parentFrame the frame in which this component is displayed
	 * @param synchronizer a UserListFunctions object which will be used for synchronize calls
	 */
	public UserListGui(Frame parentFrame, UserListFunctions synchronizer)
	{
		this.parentFrame = parentFrame;
		this.synchronizer = synchronizer;
		masterSlave = true;

		userList = new UserList();

		listGui = new JList(userList);
		listGui.setCellRenderer(new UserListCellRenderer());
		listGui.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		listGui.setBorder(new LineBorder(Color.black));

		addButton = new JButton("Add");
		addButton.addActionListener(this);
		editButton = new JButton("Edit");
		editButton.addActionListener(this);
		removeButton = new JButton("Remove");
		removeButton.addActionListener(this);

		synchButton = new JButton("Synch");
		synchButton.setIcon(new ImageIcon("img/synch.png"));
		synchButton.addActionListener(this);

		synchMasterButton = new JButton("Synch as master");
		synchMasterButton.setIcon(new ImageIcon("img/synch.png"));
		synchMasterButton.addActionListener(this);

		synchSlaveButton = new JButton("Synch as slave");
		synchSlaveButton.setIcon(new ImageIcon("img/synch.png"));
		synchSlaveButton.addActionListener(this);

		buttonPanel = new JPanel(new GridLayout(0, 1));

		buttonPanel.add(addButton);
		buttonPanel.add(editButton);
		buttonPanel.add(removeButton);
		buttonPanel.add(synchButton);
		buttonPanel.add(synchMasterButton);
		buttonPanel.add(synchSlaveButton);

		synchButton.setVisible(false);

		setLayout(new BorderLayout());
		add(listGui, BorderLayout.CENTER);
		add(buttonPanel, BorderLayout.SOUTH);

		listGui.addFocusListener(this);
	}

	/**
	 * Causes the component to switch between a two button and one button interface and act accordingly.
	 *
	 * @param masterSlave if true, show two buttons ("Synch as master", "Synch as slave"), if false, show just one
	 *	button ("Synch")
	 */
	public void setSynchButtonStyle(boolean masterSlave)
	{
		if (this.masterSlave == masterSlave)
		{
			return;
		}
		else
		{
			this.masterSlave = masterSlave;
			if (masterSlave)
			{
				synchButton.setVisible(false);
				synchMasterButton.setVisible(true);
				synchSlaveButton.setVisible(true);
			}
			else
			{
				synchButton.setVisible(true);
				synchMasterButton.setVisible(false);
				synchSlaveButton.setVisible(false);
			}
		}
	}

	public UserList getUserList()
	{
		return userList;
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getSource() == addButton)
		{
			AddEditorUserDialog aeuDialog = new AddEditorUserDialog(parentFrame);

			aeuDialog.show();

			EditorUser editorUser = new EditorUser(aeuDialog.getIp(), aeuDialog.getUsername(),
				aeuDialog.getColor());

			if (userList.addEditorUser(editorUser) == true)
			{
				synchronizer.updateText(editorUser);

				JOptionPane.showMessageDialog(parentFrame, "User added successfully.", "Info",
					JOptionPane.INFORMATION_MESSAGE);

			}
			else
			{
				JOptionPane.showMessageDialog(parentFrame, "An user with the same IP already exists.",
					"Info", JOptionPane.INFORMATION_MESSAGE);
			}
		}
		else if (e.getSource() == editButton)
		{
			int index = listGui.getSelectedIndex();

			if (index == -1)
			{
				JOptionPane.showMessageDialog(parentFrame, "Please select a list element first.",
					"Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				EditorUser editorUser = userList.getEditorUser(index);
				AddEditorUserDialog aeuDialog = new AddEditorUserDialog(parentFrame, editorUser.getIp(),
					editorUser.getUsername(), editorUser.getColor());

				aeuDialog.show();
				editorUser.setColor(aeuDialog.getColor());
				editorUser.setIp(aeuDialog.getIp());
				editorUser.setUsername(aeuDialog.getUsername());

				if (userList.setEditorUser(index, editorUser))
				{
					synchronizer.updateText(editorUser);
					JOptionPane.showMessageDialog(parentFrame, "User data successfully modified.",
						"Info", JOptionPane.INFORMATION_MESSAGE);
				}
				else
				{
					JOptionPane.showMessageDialog(parentFrame, "User data could not be modified." +
						" Try again.", "Info", JOptionPane.INFORMATION_MESSAGE);
				}
			}

		}
		else if (e.getSource() == removeButton)
		{
			int index = listGui.getSelectedIndex();

			if (index == -1)
			{
				JOptionPane.showMessageDialog(parentFrame, "Please select a list element first.",
					"Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				if (userList.removeEditorUser(index) != null)
				{
					JOptionPane.showMessageDialog(parentFrame, "User successfully removed.",
						"Info", JOptionPane.INFORMATION_MESSAGE);
				}
				else
				{
					JOptionPane.showMessageDialog(parentFrame, "No user was removed. Try again.",
						"Info", JOptionPane.INFORMATION_MESSAGE);
				}
			}
		}
		else if (e.getSource() == synchButton)
		{
			int index = listGui.getSelectedIndex();

			if (index == -1)
			{
				JOptionPane.showMessageDialog(parentFrame, "Please select a list element first.",
					"Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				EditorUser editorUser = userList.getEditorUser(index);
				synchronizer.synchronize(editorUser.getIp(), true);
			}
		}
		else if (e.getSource() == synchMasterButton)
		{
			int index = listGui.getSelectedIndex();

			if (index == -1)
			{
				JOptionPane.showMessageDialog(parentFrame, "Please select a list element first.",
					"Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				EditorUser editorUser = userList.getEditorUser(index);
				synchronizer.synchronize(editorUser.getIp(), true);
			}
		}
		else if (e.getSource() == synchSlaveButton)
		{
			int index = listGui.getSelectedIndex();

			if (index == -1)
			{
				JOptionPane.showMessageDialog(parentFrame, "Please select a list element first.",
					"Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				EditorUser editorUser = userList.getEditorUser(index);
				synchronizer.synchronize(editorUser.getIp(), false);
			}
		}
	}

	public void focusGained(FocusEvent e)
	{

	}

	public void focusLost(FocusEvent e)
	{
		if (e.getOppositeComponent() != addButton && e.getOppositeComponent() != editButton &&
			e.getOppositeComponent() != removeButton && e.getOppositeComponent() != synchButton &&
			e.getOppositeComponent() != synchSlaveButton && e.getOppositeComponent() != synchMasterButton)
		{
			listGui.clearSelection();
		}
	}

	private boolean masterSlave;

	private JButton addButton;
	private JButton editButton;
	private JButton removeButton;
	private JButton synchButton;

	private JButton synchSlaveButton;
	private JButton synchMasterButton;

	private JPanel buttonPanel;

	private JList listGui;

	private UserList userList;

	private Frame parentFrame;
	private UserListFunctions synchronizer;
}

class UserListCellRenderer extends JLabel implements ListCellRenderer
{
	public Component getListCellRendererComponent(JList list,
		Object value, // value to display
		int index, // cell index
		boolean isSelected, // is the cell selected
		boolean cellHasFocus)    // the list and the cell have the focus
	{
		EditorUser editorUser = (EditorUser) value;
		setText(editorUser.getUsername() + " (" + editorUser.getIp() + ")");

		if (isSelected)
		{
			setBackground(list.getSelectionBackground());
			setForeground(editorUser.getColor());
		}
		else
		{
			setBackground(list.getBackground());
			setForeground(editorUser.getColor());
		}

		setEnabled(list.isEnabled());
		setFont(list.getFont());
		setOpaque(true);

		return this;
	}
}