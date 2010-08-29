import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;

import java.util.List;

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

		entityManager.clear();

		final Book retrievedBook = entityManager.find(Book.class, book.getId());
		assertNotSame(book, retrievedBook);
		assertNotNull(retrievedBook);
		assertEquals("Goedel, Escher & Bach", retrievedBook.getDescription());

		entityManager.remove(retrievedBook);
		assertNull(entityManager.find(Book.class, book.getId()));
	}

	@Test
	public void addTagsToBook() {
		final Book book = new Book();
		book.setTitle("dummy");
		book.setNbOfPages(10);

		book.addTag("a");
		book.addTag("b");
		book.addTag("c");

		transaction.begin();
		entityManager.persist(book);
		transaction.commit();

		entityManager.clear();

		final Book retrievedBook = entityManager.find(Book.class, book.getId());
		assertNotSame(book, retrievedBook);

		final List<String> tags = retrievedBook.getTags();
		assertNotNull(tags);
		assertEquals(3, tags.size());
		assertTrue(tags.contains("a"));
		assertTrue(tags.contains("b"));
		assertTrue(tags.contains("c"));

		// update the DB for the book
		transaction.begin();
		retrievedBook.addTag("d");
		retrievedBook.removeTag("b");
		retrievedBook.removeTag("c");
		transaction.commit();

		entityManager.clear();

		final Book reRetrievedBook = entityManager.find(Book.class, book.getId());
		Assert.assertNotSame(retrievedBook, reRetrievedBook);

		final List<String> secondGenerationTags = reRetrievedBook.getTags();
		assertNotNull(secondGenerationTags);
		assertEquals(2, secondGenerationTags.size());
		assertTrue(tags.contains("a"));
		assertTrue(tags.contains("d"));
		assertFalse(tags.contains("b"));
		assertFalse(tags.contains("c"));
	}
}
