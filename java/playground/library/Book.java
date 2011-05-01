import java.io.Serializable;

/**
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jul 31, 2004
 */
public class Book implements Serializable
{
	static final public byte SHELF = 0;      // final - ca sa nu o pot modifiva accidental
	static final public byte HALL = 1;       // static- ca sa fie stocata intr-un singur loc , nu la fiecare obiect in parte
	static final public byte OUTSIDE = 2;    // HaALL e de fapt nr. 1

	public Book()
	{
		bookid = -1;
		title = "";
		author = "";
		shelf = -1;
		state = 3;
		reader = "";
	}

	public Book(int bookid, String title, String author, int shelf, byte state, String reader)
	{
		this.bookid = bookid;
		this.title = title;
		this.author = author;
		this.shelf = shelf;
		this.state = state;
		this.reader = reader;
	}

	public int getBookid()
	{
		return bookid;
	}

	public void setBookid(int id)           // void pt ca nu intorc nimic
	{
		bookid = id;
	}

	public String getTitle()
	{
		return title;
	}

	public void setTitle(String title)
	{
		this.title = title;
	}

	public String getAuthor()
	{
		return author;
	}

	public void setAuthor(String author)
	{
		this.author = author;
	}

	public int getShelf()
	{
		return shelf;
	}

	public void setShelf(int shelf)
	{
		this.shelf = shelf;
	}

	public byte getState()
	{
		return state;
	}

	public void setState(byte state) throws InvalidStateException   // exceptia , daca e folosita, trebuie sa apara in antetul metodei
	{
		if (state != SHELF && state != HALL && state != OUTSIDE)        //state e un parametru obtinut din exterior
		{
			throw new InvalidStateException("State was neither of shelf, hall or outside.");
		}                                               // verific ca nu atribui starii cartii o valoare invalida
		this.state = state;
	}

	public String getReader()
	{
		return reader;
	}

	public void setReader(String reader)
	{
		this.reader = reader;
	}

	public boolean equals(Object obj)
	{
		if (!(obj instanceof Book))     // verifica daca obj e de tip Book, pentru a nu compara 2 obiecte de natura diferita
			return false;

		Book tmp = (Book) obj;  // cast
		if (tmp.bookid == this.bookid)
			return true;
		else
			return false;
	}

	public String toString()
	{
		String strRepresentation = bookid + ": " + author + ", " + title + " - shelf " + shelf + ", ";
		if (state == SHELF)
		{
			strRepresentation += "on shelf";
		}
		else if (state == HALL)
		{
			strRepresentation += "in the hall";
		}
		else if (state == OUTSIDE)
		{
			strRepresentation += "outside";
		}

		strRepresentation += ", reader name: " + reader;

		return strRepresentation;
	}

	private int bookid;
	private String title;
	private String author;
	private int shelf;
	private byte state;     // folosesc byte in loc de int deoarece ocupa mai putin spatiu
				// stare va avea valorile 0,1,2
	private String reader;
}
