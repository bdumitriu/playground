package ass1.view;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JTable;

import ass1.controller.Controller;
import ass1.model.Book;

public class ListBooksPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	public ListBooksPanel(final Controller controller) {
		Book[] books = controller.getBooks();
		Object[][] rowData = new Object[books.length][5];
		for (int i = 0; i < books.length; i++) {
			rowData[i][0] = books[i].getTitle();
			rowData[i][1] = books[i].getAuthor();
			rowData[i][2] = books[i].getIsbn();
			rowData[i][3] = books[i].getPublisher().getName();
			rowData[i][4] = books[i].getNrPages();
		}
		Object[] columnNames = new String[] {
				"Title", "Author", "ISBN", "Publisher", "Nr. Pages"};
		JTable table = new JTable(rowData, columnNames);

		add(table.getTableHeader(), BorderLayout.NORTH);
		add(table, BorderLayout.CENTER);
	}
}
