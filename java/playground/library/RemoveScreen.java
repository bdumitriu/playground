
import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Aug 15, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class RemoveScreen extends JPanel implements ActionListener
{
	public RemoveScreen(LibraryGui libraryGui)
	{
		this.libraryGui = libraryGui;

		JLabel bookIdLabel = new JLabel("Id of book to remove: ");

		bookIdField = new JTextField();

		JButton okButton = new JButton("Ok");
		okButton.addActionListener(this);

		setLayout(new GridLayout(2, 2));
		add(bookIdLabel);
		add(bookIdField);
		add(okButton);
	}

	public void actionPerformed(ActionEvent e)
	{
		String bookIdString = bookIdField.getText();

		int bookId = -1;
		try
		{
			bookId = new Integer(bookIdString).intValue();
		}
		catch (NumberFormatException ex)
		{
			JOptionPane.showMessageDialog(libraryGui, "Book id has to be a valid number.", "Error",
				JOptionPane.ERROR_MESSAGE);
			return;
		}

		Book removedBook = libraryGui.getLibrary().removeBook(bookId);

		if (removedBook == null)
		{
			JOptionPane.showMessageDialog(libraryGui, "No book with supplied id was found.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		else
		{
			JOptionPane.showMessageDialog(libraryGui, "Book with id " + bookId + " is was removed from " +
				"the library.", "Success", JOptionPane.INFORMATION_MESSAGE);

			bookIdField.setText("");
		}
	}

	private JTextField bookIdField;

	private LibraryGui libraryGui;
}