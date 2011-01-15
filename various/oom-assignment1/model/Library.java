package ass1.model;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;

public class Library implements Serializable {

	private static final long serialVersionUID = 1L;

	private transient String libraryFile;

	private ArrayList<Book> books;

	public Library(String libraryFile) {
		File f = new File(libraryFile);
		if (f.exists() && f.canRead()) {
			try {
				FileInputStream fis = new FileInputStream(f);
				ObjectInputStream ois = new ObjectInputStream(fis);
				Library lib = (Library) ois.readObject();
				books = lib.books;
			} catch (Exception e) {
				books = new ArrayList<Book>();
				e.printStackTrace();
			}
		} else {
			books = new ArrayList<Book>();
		}

		this.libraryFile = libraryFile;
	}

	public synchronized void saveToFile() {
		File f = new File(libraryFile);
		try {
			FileOutputStream fos = new FileOutputStream(f);
			ObjectOutputStream oos = new ObjectOutputStream(fos);
			oos.writeObject(this);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public synchronized void addBook(Book book) {
		books.add(book);
	}

	public Book[] getBooks() {
		return books.toArray(new Book[0]);
	}
}
