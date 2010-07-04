import javax.persistence.*;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
public class Main {

	public static void main(String[] args) {
		Book book = new Book();

		book.setTitle("GEB");
		book.setPrice(12.5F);
		book.setDescription("Goedel, Escher & Bach - an eternal golden braid");
		book.setIsbn("111-222-333-444");
		book.setNbOfPages(789);
		book.setIllustrations(false);

		final EntityManagerFactory entityManagerFactory = Persistence.createEntityManagerFactory("chapter02PU");
		final EntityManager entityManager = entityManagerFactory.createEntityManager();

		final EntityTransaction transaction = entityManager.getTransaction();
		transaction.begin();
		entityManager.persist(book);
		/*final Query query = entityManager.createNamedQuery("findAllBooks");
		final List resultList = query.getResultList();
		for (Object o : resultList) {
			Book b = (Book) o;
			System.out.println(String.format("%3d: %s", b.getId(), b.getDescription()));
		}*/
		transaction.commit();

		entityManager.close();
		entityManagerFactory.close();
	}
}
