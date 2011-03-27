import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Bogdan Dumitriu
 */
public class ArtistTest {

	private static EntityManagerFactory entityManagerFactory;

	private static EntityManager entityManager;

	private EntityTransaction transaction;

	@BeforeClass
	public static void createEntityManager() {
		entityManagerFactory = Persistence.createEntityManagerFactory("chapter03PUTest");
		entityManager = entityManagerFactory.createEntityManager();
	}

	@AfterClass
	public static void closeEntityManger() {
		entityManager.close();
		entityManagerFactory.close();
	}

	@Before
	public void startTransaction() {
		transaction = entityManager.getTransaction();
	}

	@Test
	public void testManyToMany() {
		final Artist cohen = new Artist();
		cohen.setFirstName("Leonard");
		cohen.setLastName("Cohen");
		final Artist cash = new Artist();
		cash.setFirstName("Johnny");
		cash.setLastName("Cash");

		final CD cohenAndCashCd = new CD();
		cohenAndCashCd.setTitle("Cohen & Cash");
		cohenAndCashCd.setPrice(1.0F);
		final CD cohenOnlyCd = new CD();
		cohenOnlyCd.setTitle("Cohen Best Hits");
		cohenOnlyCd.setPrice(1.0F);
		final CD cashOnlyCd = new CD();
		cashOnlyCd.setTitle("Cash Best Hits");
		cashOnlyCd.setPrice(1.0F);

		final LinkedList<CD> cohenCds = new LinkedList<CD>();
		Collections.addAll(cohenCds, cohenAndCashCd, cohenOnlyCd);
		cohen.setAppearsOn(cohenCds);
		final LinkedList<CD> cashCds = new LinkedList<CD>();
		Collections.addAll(cashCds, cohenAndCashCd, cashOnlyCd);
		cash.setAppearsOn(cashCds);

		transaction.begin();
		entityManager.persist(cohenAndCashCd);
		entityManager.persist(cohenOnlyCd);
		entityManager.persist(cashOnlyCd);
		entityManager.persist(cohen);
		entityManager.persist(cash);
		transaction.commit();

		entityManager.clear();
		entityManagerFactory.getCache().evictAll();

		checkCd(cohenAndCashCd, cohen, cash);
		checkCd(cohenOnlyCd, cohen);
		checkCd(cashOnlyCd, cash);
	}

	private void checkCd(CD cd, Artist... artists) {
		final CD retrievedCd = entityManager.find(CD.class, cd.getId());
		assertNotSame(cd, retrievedCd);

		final List<Artist> retrievedArtists = retrievedCd.getCreatedBy();
		assertEquals(artists.length, retrievedArtists.size());
		for (int i = 0; i < artists.length; i++) {
			checkArtist(artists[i], retrievedArtists.get(i));
		}
	}

	private void checkArtist(Artist artist, Artist retrievedArtist) {
		assertEquals(artist.getFirstName(), retrievedArtist.getFirstName());
		assertEquals(artist.getLastName(), retrievedArtist.getLastName());
	}
}
