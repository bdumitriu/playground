import javax.ejb.Remote;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Remote
public interface ItemRemote {

	List<Book> findBooks();

	List<CD> findCDs();

	Book createBook(Book book);

	CD createCD(CD cd);
}
