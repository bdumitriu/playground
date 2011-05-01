import java.util.ArrayList;
import java.util.Iterator;
import java.io.*;

/**
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jul 31, 2004
 */
public class Library
{
	public static final String FILENAME = "/home/bdumitriu/IdeaProjects/library.dat";

	public Library()
	{
		books = new ArrayList();
		readBooks();
	}

	public int addBook(String title, String author, int shelf) // adauga o carte in lista
	{
		Book book;      // declar var.  conteaa ordinea
		book = new Book(lastId, title, author, shelf, Book.SHELF, "");      //initializez
		lastId++; // book.setBookid(lastId) si lasam -1 in paranteza
		books.add(book);
		return lastId - 1;
	}

	public Book findBook(int bookId)        //intoarce obiectul de tip Book cu cota bookId-param. functiei
	{
		Book book = new Book();
		book.setBookid(bookId); //seteaza cota la valoarea bookId


		int bookIndex = books.indexOf(book);                    // bookIndex = indexul cartii gasite in lista

		if (bookIndex == -1)
			return null;
		else

		{
			Object returnedObject = books.get(bookIndex);           // obiectul corespunzator indexului
			Book bookToReturn = (Book) returnedObject;              // cast
			return bookToReturn;
		}
	}

	public boolean modifyBook(Book book)       // modific un obiect carte vechi pe baza id-ului lui book existent
	{
		try
		{
			int bookIndex = books.indexOf(book);
			if (bookIndex == -1)
			{
				return false;
			}
			else
			{
				books.set(bookIndex, book);
				return true;
			}
		}
		catch (IndexOutOfBoundsException e)
		{
			return false;
		}
	}

	public Book removeBook(int bookId)
	{
		try
		{
			Book book = new Book();
			book.setBookid(bookId); // setez id-ul cartii de lucru la valoarea parametrului bookId
			return (Book) books.remove(books.indexOf(book)); // obtin index, pe baza caruia sterg cartea, functia remove
		}  // imi ofera obiectul sters, pe care il convertesc
		catch (IndexOutOfBoundsException e)
		{
			return null;
		}
	}

	public void listBooks()
	{
		for (int i = 0; i < books.size(); i++)
		{
			Book b = (Book) books.get(i);
			System.out.println(b.toString());
		}
	}

	public Iterator getBookIterator()
	{
		return books.iterator();
	}

	public int getNrBooks()
	{
		return books.size();
	}

	public boolean borrowBook(int bookId, String reader)
	{
		try
		{
			Book book = findBook(bookId);
			if (book == null)
				return false;
			if (book.getState() == Book.SHELF)
			{
				book.setState(Book.OUTSIDE);            // variabila (metoda) statica e apelata prin intermediul clasei
				book.setReader(reader);
				return true;
			}
			else
				return false;
		}
		catch (InvalidStateException e)
		{
			return false;				// stiu ca nu am cum sa primesc exceptie deoarece dau variabila OUTSIDE ca parametru
		}
	}

	public boolean returnBook(int bookId)
	{
		try
		{
			Book book = findBook(bookId);
			if (book == null)
				return false;

			book.setState(Book.SHELF);            // variabila (metoda) statica e apelata prin intermediul clasei
			book.setReader("");
			return true;
		}
		catch (InvalidStateException e)
		{
			return false;				// stiu ca nu am cum sa primesc exceptie deoarece dau variabila OUTSIDE ca parametru
		}
	}

	public void saveBooks()
	{
		try
		{
			FileOutputStream fos = new FileOutputStream(FILENAME);  // creez un stream pt a putea scrie in fisierul declarat mai sus
			ObjectOutputStream oos = new ObjectOutputStream(fos);   // are ca scop sa intermedieze scrierea de obiecte in stream de unde sunt directionate catre fisier

			oos.writeInt(lastId);   // scriu intreg
			oos.writeInt(books.size());

			for (int i = 0; i < books.size(); i++)
			{
				oos.writeObject(books.get(i));
			}

			oos.close();
		}
		catch (FileNotFoundException e)
		{
			System.out.println(e.getMessage());
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
		}
	}

	public void readBooks()
	{
		try
		{
			FileInputStream fis = new FileInputStream(FILENAME);
			ObjectInputStream ois = new ObjectInputStream(fis);

			lastId = ois.readInt();
			int nrOfBooks = ois.readInt();

			books.clear();

			for (int i = 0; i < nrOfBooks; i++)
			{
				books.add(ois.readObject());
			}

			ois.close();
		}
		catch (FileNotFoundException e)
		{
			System.out.println(e.getMessage());
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
		}
		catch (ClassNotFoundException e)
		{
			System.out.println(e.getMessage());
		}
	}

	private ArrayList books;        // ArrayList e o clasa ce poate contine obiecte de orice fel de tip
	private int lastId;
}
