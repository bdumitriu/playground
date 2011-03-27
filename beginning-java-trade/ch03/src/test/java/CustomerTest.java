import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;

import org.junit.*;

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
	public void testOneToOne() {
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

		entityManager.clear();
		entityManagerFactory.getCache().evictAll();

		final Customer retrievedCustomer = entityManager.find(Customer.class, customer.getId());
		assertNotSame(customer, retrievedCustomer);

		final Address retrievedAddress = retrievedCustomer.getAddress();
		assertEquals(address.getStreet1(), retrievedAddress.getStreet1());
		assertEquals(address.getCity(), retrievedAddress.getCity());
		assertEquals(address.getCountry(), retrievedAddress.getCountry());
	}
}
