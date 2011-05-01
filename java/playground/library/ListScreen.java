
import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.util.Iterator;
import java.awt.*;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Aug 1, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class ListScreen extends JPanel
{
	public ListScreen(LibraryGui libraryGui)
	{
		this.libraryGui = libraryGui;
	}

	public void updateBookList()  // sa actualizeze lista in momentul imediat urmator introducerii unei carti
	{
		Object[][] data = new Object[libraryGui.getLibrary().getNrBooks()][7];  // le fac de tip Object pt ca sunt si de tip intreg si String

		Iterator it = libraryGui.getLibrary().getBookIterator();
		int i = 0;
		while (it.hasNext() == true)
		{
			Book book = (Book) it.next();

			data[i][0] = new Integer(i+1);   // data e o matrice de obiecte
			data[i][1] = new Integer(book.getBookid());
			data[i][2] = book.getTitle();
			data[i][3] = book.getAuthor();
			byte state = book.getState();
			if (state == Book.SHELF)
				data[i][4] = "on shelf";
			else if (state == Book.HALL)
				data[i][4] = "in hall";
			else if (state == Book.OUTSIDE)
				data[i][4] = "outside";
			data[i][5] = new Integer(book.getShelf());
			data[i][6] = book.getReader();
			i++;
		}

		String[] cnames = new String[7];
		cnames[0] = "Current Number";
		cnames[1] = "Book ID";
		cnames[2] = "Title";
		cnames[3] = "Author";
		cnames[4] = "State";
		cnames[5] = "Shelf Number";
		cnames[6] = "Reader Name";

		MyTableModel myTM = new MyTableModel(data, cnames);
		JTable table = new JTable(myTM);

		JScrollPane scrollPane = new JScrollPane(table);	// tabelul e bagat intr-un scrollPane - permite afis capurilor de tabel (ca un chenar)

		setLayout(new GridLayout(1,1));

		removeAll();
		add(scrollPane);
	}

	private LibraryGui libraryGui;
}

class MyTableModel extends DefaultTableModel
{
	public MyTableModel(Object[][] rowData, Object[] columnNames)
	{
		super(rowData, columnNames);
	}

	public boolean isCellEditable(int row, int column)
	{
		return false;
	}
}