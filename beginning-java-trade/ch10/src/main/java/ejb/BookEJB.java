package ejb;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import java.util.List;

import entitiy.Book;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class BookEJB {

	@PersistenceContext(unitName = "chapter10PU")
	private EntityManager entityManager;

	public List<Book> findBooks() {
		final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		final CriteriaQuery<Book> criteriaQuery = criteriaBuilder.createQuery(Book.class);
		criteriaQuery.from(Book.class);
		final TypedQuery<Book> query = entityManager.createQuery(criteriaQuery);
		return query.getResultList();
	}

	public Book createBook(Book book) {
		entityManager.persist(book);
		return book;
	}
}
