import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;
import java.util.Map;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Bogdan Dumitriu
 */
public class CDTest {

	private static EntityManagerFactory entityManagerFactory;

	private static EntityManager entityManager;

	private EntityTransaction transaction;

	@BeforeClass
	public static void initEntityManager() {
		entityManagerFactory = Persistence.createEntityManagerFactory("chapter03PUTest");
		entityManager = entityManagerFactory.createEntityManager();
	}

	@AfterClass
	public static void closeEntityManager() {
		entityManager.close();
		entityManagerFactory.close();
	}

	@Before
	public void startTransaction() {
		transaction = entityManager.getTransaction();
	}

	@Test
	public void checkTracksAreRetrievedCorrectly() {
		final CD cd = new CD();
		cd.setTitle("my cd");
		cd.setPrice(5.99F);

		cd.putTrack(1, "track 1");
		cd.putTrack(2, "track 2");
		cd.putTrack(3, "track 3");

		transaction.begin();
		entityManager.persist(cd);
		transaction.commit();

		entityManager.clear();
		entityManagerFactory.getCache().evictAll();

		final CD retrievedCd = entityManager.find(CD.class, cd.getId());
		assertNotSame(cd, retrievedCd);

		final Map<Integer,String> tracks = retrievedCd.getTracks();
		assertNotNull(tracks);
		assertEquals(3, tracks.size());
		assertTrue(tracks.containsKey(1));
		assertEquals("track 1", tracks.get(1));
		assertTrue(tracks.containsKey(2));
		assertEquals("track 2", tracks.get(2));
		assertTrue(tracks.containsKey(3));
		assertEquals("track 3", tracks.get(3));
	}
}
