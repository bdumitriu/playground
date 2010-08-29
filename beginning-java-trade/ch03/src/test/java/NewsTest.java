import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;

import javax.persistence.*;
import javax.persistence.criteria.CriteriaQuery;

import org.junit.*;

/**
 * @author Bogdan Dumitriu
 */
public class NewsTest {

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
	public void createNews() {
		final News news = new News();
		news.setId(new NewsId("Reforma expertizelor judiciare - Propuneri noi care bat pasul pe loc", "RO"));
		news.setContent("Acum citeva luni scriam de bulibaseala nationala in materia expertizei in IT.");

		transaction.begin();
		entityManager.persist(news);
		transaction.commit();

		assertNotNull("ID should not be null", news.getId());

		entityManager.clear();

		final News retrievedNews = entityManager.find(News.class,
				new NewsId("Reforma expertizelor judiciare - Propuneri noi care bat pasul pe loc", "RO"));
		assertNotSame(news, retrievedNews);
		assertNotNull(retrievedNews);
		assertEquals("Acum citeva luni scriam de bulibaseala nationala in materia expertizei in IT.",
				retrievedNews.getContent());

		entityManager.remove(retrievedNews);
		assertNull(entityManager.find(News.class,
				new NewsId("Reforma expertizelor judiciare - Propuneri noi care bat pasul pe loc", "RO")));
	}

	@Test
	public void queryNewsUsingJpql() {
		final News news = new News();
		news.setId(new NewsId("Creditorii Catavencu SA au ales falimentul companiei", "RO"));
		news.setContent("Compania Catavencu SA intra in faliment.");

		transaction.begin();
		entityManager.persist(news);
		transaction.commit();

		final Query query = entityManager.createQuery("select n.id.title from News n");
		final String retrievedTitle = (String) query.getSingleResult();
		assertEquals("Creditorii Catavencu SA au ales falimentul companiei", retrievedTitle);
	}
}
