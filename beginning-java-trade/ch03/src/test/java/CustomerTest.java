import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Bogdan Dumitriu
 */
public class CustomerTest {

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
	public void testAddressOneToOne() {
		final Customer customer = new Customer();
		customer.setFirstName("Sam");
		customer.setLastName("Sample");
		final Address address = new Address();
		address.setStreet1("54, 84th Street");
		address.setCity("New York");
		address.setCountry("USA");
		customer.setAddress(address);

		transaction.begin();
		entityManager.persist(address);
		entityManager.persist(customer);
		transaction.commit();

		
	}
}
