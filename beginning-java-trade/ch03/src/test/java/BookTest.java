import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;

import org.junit.*;

/**
 * @author Bogdan Dumitriu
 */
public class BookTest {

	private static EntityManagerFactory entityManagerFactory;

	private static EntityManager entityManager;

	private EntityTransaction transaction;

	@BeforeClass
	public static void initEntityManager() {
		entityManagerFactory = Persistence.createEntityManagerFactory("chapter03PUTest");
		entityManager = entityManagerFactory.createEntityManager();
	}

	@AfterClass
	public static void closeEntityManager() throws Exception {
		entityManager.close();
		entityManagerFactory.close();
	}

	@Before
	public void startTransaction() {
		transaction = entityManager.getTransaction();
	}

	@Test
	public void createBook() {
		final Book book = new Book();
		book.setTitle("GEB");
		book.setPrice(12.5F);
		book.setDescription("Goedel, Escher & Bach");
		book.setIsbn("111-222-333-444");
		book.setNbOfPages(789);
		book.setIllustrations(false);

		transaction.begin();
		entityManager.persist(book);
		transaction.commit();

		assertNotNull("ID should not be null", book.getId());

		final Book retrievedBook = entityManager.find(Book.class, book.getId());
		assertNotNull(retrievedBook);
		assertEquals("Goedel, Escher & Bach", retrievedBook.getDescription());

		entityManager.remove(book);
		assertNull(entityManager.find(Book.class, book.getId()));
	}
}
