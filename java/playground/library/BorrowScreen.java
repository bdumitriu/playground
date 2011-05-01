
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
public class BorrowScreen extends JPanel implements ActionListener
{
	public BorrowScreen(LibraryGui libraryGui)
	{
		this.libraryGui = libraryGui;

		JLabel bookIdLabel = new JLabel("Id of book being borrowed: ");
		JLabel readerLabel = new JLabel("Reader name");

		bookIdField = new JTextField();
		readerField = new JTextField();

		JButton okButton = new JButton("Ok");
		okButton.addActionListener(this);

		setLayout(new GridLayout(3, 2));
		add(bookIdLabel);
		add(bookIdField);
		add(readerLabel);
		add(readerField);
		add(okButton);
	}

	public void actionPerformed(ActionEvent e)
	{
		String bookIdString = bookIdField.getText();
		String reader = readerField.getText();

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

		boolean result = libraryGui.getLibrary().borrowBook(bookId, reader);

		if (result == false)
		{
			JOptionPane.showMessageDialog(libraryGui, "No book with supplied id was found or specified " +
				"book is currently borrowed.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		else
		{
			JOptionPane.showMessageDialog(libraryGui, "Book with id " + bookId + " is now marked as " +
				"\"borrowed\"", "Success", JOptionPane.INFORMATION_MESSAGE);

			bookIdField.setText("");
			readerField.setText("");
		}
	}

	private JTextField bookIdField;
	private JTextField readerField;

	private LibraryGui libraryGui;
}
