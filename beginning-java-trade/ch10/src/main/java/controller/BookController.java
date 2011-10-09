package controller;

import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import java.util.ArrayList;
import java.util.List;

import ejb.BookEJB;
import entitiy.Book;

/**
 * @author Bogdan Dumitriu
 */
@ManagedBean
@RequestScoped
public class BookController {

	@EJB
	private BookEJB bookEJB;

	private Book book = new Book();

	private List<Book> bookList = new ArrayList<Book>();

	public String doNew() {
		return "newBook.xhtml";
	}

	public String doCreateBook() {
		book = bookEJB.createBook(book);
		bookList = bookEJB.findBooks();
//		return "listBooks.xhtml";
//		return "listBooks_oldStyle.jsp";
		return "listBooks_tagLib.jsp";
	}

	public Book getBook() {
		return book;
	}

	public void setBook(Book book) {
		this.book = book;
	}

	public List<Book> getBookList() {
		return bookList;
	}

	public void setBookList(List<Book> bookList) {
		this.bookList = bookList;
	}
}
