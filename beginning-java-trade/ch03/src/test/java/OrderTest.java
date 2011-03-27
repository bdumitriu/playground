import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.Persistence;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Bogdan Dumitriu
 */
public class OrderTest {

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
	public void testOneToMany() {
		final Order order = new Order();
		order.setCreationDate(new Date(System.currentTimeMillis()));
		final OrderLine orderLine1 = new OrderLine("Leonard Cohen - 10 New Songs", 5.99, 1);
		final OrderLine orderLine2 = new OrderLine("Nirvana - Unplugged in New York", 15.99, 5);
		final LinkedList<OrderLine> orderLines = new LinkedList<OrderLine>();
		Collections.addAll(orderLines, orderLine1, orderLine2);
		order.setOrderLines(orderLines);

		transaction.begin();
		entityManager.persist(order);
		transaction.commit();

		entityManager.clear();
		entityManagerFactory.getCache().evictAll();

		final Order retrievedOrder = entityManager.find(Order.class, order.getId());
		assertNotSame(order, retrievedOrder);

		final List<OrderLine> retrievedOrderLines = retrievedOrder.getOrderLines();
		assertEquals(2, retrievedOrderLines.size());
		checkOrderLine(orderLine1, retrievedOrderLines.get(0));
		checkOrderLine(orderLine2, retrievedOrderLines.get(1));
	}

	private void checkOrderLine(OrderLine orderLine, OrderLine retrievedOrderLine) {
		assertEquals(orderLine.getItem(), retrievedOrderLine.getItem());
		assertEquals(orderLine.getUnitPrice(), retrievedOrderLine.getUnitPrice());
		assertEquals(orderLine.getQuantity(), retrievedOrderLine.getQuantity());
	}
}
