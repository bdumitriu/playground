import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Aug 1, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class EditScreen extends JPanel implements ActionListener           //  ca clasa sa inst un obiect de tip component
{
	private final static String saveId = "save";
	private final static String searchId = "search";

	public EditScreen(LibraryGui libraryGui)
	{
		this.libraryGui = libraryGui;
		book = null;

		JLabel titleLabel = new JLabel("Title: ");
		JLabel authorLabel = new JLabel("Author: ");
		JLabel shelfLabel = new JLabel("Shelf: ");
		JLabel readerLabel = new JLabel("Reader: ");

		bookIdField = new JTextField();
		titleField = new JTextField();
		authorField = new JTextField();  // initializare prin creare de obiect
		shelfField = new JTextField();
		readerField = new JTextField();

		JButton searchButton = new JButton("Search for book");
		JButton saveButton = new JButton("Save changes");

		GridLayout layout = new GridLayout(6, 2);  // layout manevreaza aranjarea ob, el e un maneger
		setLayout(layout);  // se refera la obiect this (de tip JPanel)
		add(bookIdField);
		add(searchButton);
		add(titleLabel);
		add(titleField);
		add(authorLabel);
		add(authorField);
		add(shelfLabel);
		add(shelfField);
		add(readerLabel);
		add(readerField);

		add(saveButton);

		saveButton.setActionCommand(saveId);
		searchButton.setActionCommand(searchId);
		saveButton.addActionListener(this);
		searchButton.addActionListener(this);
	}

	public void reset()
	{
		book = null;

		bookIdField.setText("");
		titleField.setText("");
		authorField.setText("");
		readerField.setText("");
		shelfField.setText("");
	}

	public void actionPerformed(ActionEvent e)
	{
		String id = e.getActionCommand();
		if (id.equals(saveId))
		{
			if (book == null)
			{
				JOptionPane.showMessageDialog(libraryGui, "Error: No valid book has been retrieved for modification yet.", "Error", JOptionPane.ERROR_MESSAGE);
				return;
			}

			String title = titleField.getText();
			String author = authorField.getText();
			String tmp = shelfField.getText();
			String reader = readerField.getText();
			int shelf = -1;
			try
			{
				shelf = Integer.parseInt(tmp);
			}
			catch (NumberFormatException f)
			{
				JOptionPane.showMessageDialog(libraryGui, "Error: Shelf was not a number.", "Error", JOptionPane.ERROR_MESSAGE);
				return;
			}

			tmp = bookIdField.getText();
			int bookId = -1;
			try
			{
				bookId = Integer.parseInt(tmp);
			}
			catch (NumberFormatException ex)
			{
				JOptionPane.showMessageDialog(libraryGui, "Error: Book id was not a number.", "Error", JOptionPane.ERROR_MESSAGE);
				return;
			}

			Book modBook = new Book(bookId, title, author, shelf, book.getState(), reader);
			if (libraryGui.getLibrary().modifyBook(modBook) == false)
			{
				JOptionPane.showMessageDialog(libraryGui, "Error: No book with specified id was found.", "Error", JOptionPane.ERROR_MESSAGE);
			}
			else
			{
				JOptionPane.showMessageDialog(libraryGui, "Book details were successfully updated.", "Succes", JOptionPane.INFORMATION_MESSAGE);
			}
		}
		else if (id.equals(searchId))
		{
			String bookIdString = bookIdField.getText();
			if (bookIdString.equals(""))
			{
				JOptionPane.showMessageDialog(libraryGui, "Error: Book id field is empty.", "Error", JOptionPane.ERROR_MESSAGE);
			}
			else
			{
				int bookId = -1;
				try
				{
					bookId = Integer.parseInt(bookIdString);

				}
				catch (NumberFormatException ex)
				{
					bookId = -1;
				}

				if (bookId < 0)
				{
					JOptionPane.showMessageDialog(libraryGui, "Error: Book id was not a positive number.", "Error", JOptionPane.ERROR_MESSAGE);
				}
				else
				{
					book = libraryGui.getLibrary().findBook(bookId);

					if (book == null)
					{
						JOptionPane.showMessageDialog(libraryGui, "Error: No book with supplied id exists in the library.", "Error", JOptionPane.ERROR_MESSAGE);
					}
					else
					{
						titleField.setText(book.getTitle());
						authorField.setText(book.getAuthor());
						shelfField.setText(new Integer(book.getShelf()).toString());
						readerField.setText(book.getReader());
					}
				}
			}
		}
	}

	private Book book;

	private JTextField bookIdField;
	private JTextField titleField;
	private JTextField authorField;		//  declaratie de obiect
	private JTextField readerField;
	private JTextField shelfField;
	private LibraryGui libraryGui;
}
