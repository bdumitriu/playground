import javax.ejb.Local;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Local
public interface ItemLocal {

	List<Book> findBooks();

	List<CD> findCDs();
}
