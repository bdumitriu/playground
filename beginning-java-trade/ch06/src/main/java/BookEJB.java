import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean // unless @LocalBean is present, the JNDI lookup in BookEJBTest fails
public class BookEJB implements BookEJBRemote {

	@PersistenceContext(unitName = "chapter06PU")
	private EntityManager entityManager;

	@Override
	public List<Book> findBooks() {
		final TypedQuery<Book> query = entityManager.createNamedQuery(Book.FIND_ALL_BOOKS, Book.class);
		return query.getResultList();
	}

	@Override
	public Book findBookById(Long id) {
		return entityManager.find(Book.class, id);
	}

	@Override
	public Book createBook(Book book) {
		entityManager.persist(book);
		return book;
	}

	@Override
	public void deleteBook(Book book) {
		entityManager.remove(entityManager.merge(book));
	}

	@Override
	public Book updateBook(Book book) {
		return entityManager.merge(book);
	}
}
