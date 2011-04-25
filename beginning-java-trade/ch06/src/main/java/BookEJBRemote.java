import javax.ejb.Remote;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Remote
public interface BookEJBRemote {

	List<Book> findBooks();

	Book findBookById(Long id);

	Book createBook(Book book);

	void deleteBook(Book book);

	public Book updateBook(Book book);
}
