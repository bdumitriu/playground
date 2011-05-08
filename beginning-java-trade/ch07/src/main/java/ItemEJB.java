import javax.annotation.Resource;
import javax.ejb.LocalBean;
import javax.ejb.SessionContext;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class ItemEJB implements ItemLocal, ItemRemote {

	@PersistenceContext(unitName = "chapter07PU")
	private EntityManager entityManager;

	@Resource
	private SessionContext sessionContext;

	public List<Book> findBooks() {
		final TypedQuery<Book> query = entityManager.createNamedQuery(Book.FIND_ALL_BOOKS, Book.class);
		return query.getResultList();
	}

	public List<CD> findCDs() {
		final TypedQuery<CD> query = entityManager.createNamedQuery(CD.FIND_ALL_CDS, CD.class);
		return query.getResultList();
	}

	public Book createBook(Book book) {
		entityManager.persist(book);
		return book;
	}

	public CD createCD(CD cd) {
		entityManager.persist(cd);
		return cd;
	}

	public void removeBook(Book book) {
		entityManager.remove(entityManager.merge(book));
	}

	public void removeCD(CD cd) {
		entityManager.remove(entityManager.merge(cd));
	}

	public void alwaysFailingCreateBook(Book book) {
		entityManager.persist(book);
		sessionContext.setRollbackOnly();
	}
}
