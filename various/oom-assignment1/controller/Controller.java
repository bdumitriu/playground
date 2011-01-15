package ass1.controller;

import ass1.model.Book;
import ass1.model.Library;
import ass1.model.Publisher;

public class Controller {

	private Library library;

	public Controller(Library library) {
		this.library = library;
	}

	public void save() {
		library.saveToFile();
	}

	public void addBook(String title, String author, String isbn,
			String publisher, int nrPages) {
		Book book = new Book();
		book.setTitle(title);
		book.setAuthor(author);
		book.setNrPages(nrPages);
		book.setIsbn(isbn);
		book.setPublisher(new Publisher(publisher));
		library.addBook(book);
	}

	public Book[] getBooks() {
		return library.getBooks();
	}
}
