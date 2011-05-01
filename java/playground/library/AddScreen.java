
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
public class AddScreen extends JPanel implements ActionListener           //  ca clasa sa inst un obiect de tip component
{
	public AddScreen(LibraryGui libraryGui)
	{
		this.libraryGui = libraryGui;

		JLabel titleLabel = new JLabel("Title: ");
		JLabel authorLabel = new JLabel("Author: ");
		JLabel shelfLabel = new JLabel("Shelf: ");

		titleField = new JTextField();
		authorField = new JTextField();  // initializare prin creare de obiect
		shelfField = new JTextField();

		JButton okButton = new JButton("Ok");

		GridLayout layout = new GridLayout(4, 2);  // layout manevreaza aranjarea ob, el e un maneger
		setLayout(layout);  // se refera la obiect this (de tip JPanel)
		add(titleLabel);
		add(titleField);
		add(authorLabel);
		add(authorField);
		add(shelfLabel);
		add(shelfField);
		add(okButton);

		okButton.addActionListener(this);
	}

	public void actionPerformed(ActionEvent e)
	{
		String title = titleField.getText();
		String author = authorField.getText();
		String tmp = shelfField.getText();
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
		int bookId = libraryGui.getLibrary().addBook(title, author, shelf);

		titleField.setText("");
		authorField.setText("");
		shelfField.setText("");

		JOptionPane.showMessageDialog(libraryGui, "Book was successfully added with id " + bookId + ".", "Succes", JOptionPane.INFORMATION_MESSAGE);
	}

	private JTextField titleField;
	private JTextField authorField;		//  declaratie de obiect
	private JTextField shelfField;
	private LibraryGui libraryGui;
}
