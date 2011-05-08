import static org.junit.Assert.assertEquals;

import javax.ejb.embeddable.EJBContainer;
import javax.naming.Context;
import javax.naming.NamingException;
import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Bogdan Dumitriu
 */
public class ShoppingCartTest {

	private static EJBContainer ejbContainer;

	private static Context context;

	@BeforeClass
	public static void initContainer() {
		final Map<String, Object> properties = new HashMap<String, Object>();
		properties.put(EJBContainer.APP_NAME, "ch07Test");
		properties.put(EJBContainer.MODULES, new File("target/classes"));
		// this makes embedded glassfish use the domain.xml that contains the Chapter07Pool resource configuration
		properties.put("org.glassfish.ejb.embedded.glassfish.installation.root", "./target/test-classes/glassfish");
		ejbContainer = EJBContainer.createEJBContainer(properties);
		context = ejbContainer.getContext();
	}

	@AfterClass
	public static void closeContainer() {
		ejbContainer.close();
	}

	@Test
	public void testShoppingCart() throws NamingException {
		// java:global[/<appName>]/<moduleName>/<beanName>[!<fullyQualifiedInterfaceName>]
		final ItemEJB itemEJB = (ItemEJB) context.lookup("java:global/ch07Test/classes/ItemEJB!ItemEJB");

		final Book book = new Book();
		book.setTitle("The Game of Thrones");
		book.setPrice(19.99F);
		book.setDescription("Fantasy book by GRRM");
		book.setIsbn("978-0553573404");
		book.setNbOfPages(831);

		itemEJB.createBook(book);

		final CD cd = new CD();
		cd.setTitle("Pulp Fiction Soundtrack");
		cd.setPrice(9.99F);

		itemEJB.createCD(cd);

		// java:global[/<appName>]/<moduleName>/<beanName>[!<fullyQualifiedInterfaceName>]
		final ShoppingCartEJB shoppingCartEJB =
				(ShoppingCartEJB) context.lookup("java:global/ch07Test/classes/ShoppingCartEJB!ShoppingCartEJB");
		final ItemConverterEJB itemConverterEJB =
				(ItemConverterEJB) context.lookup("java:global/ch07Test/classes/ItemConverterEJB!ItemConverterEJB");

		final float exchangeRate = itemConverterEJB.getExchangeRate();
		final float expectedTotal = (book.getPrice() + cd.getPrice()) * exchangeRate;

		shoppingCartEJB.addItem(book);
		shoppingCartEJB.addItem(cd);

		assertEquals((double) expectedTotal, (double) shoppingCartEJB.getTotal(), 0.001);
		shoppingCartEJB.checkout();
		assertEquals(0.0, (double) shoppingCartEJB.getTotal(), 0.001);

		itemEJB.removeBook(book);
		itemEJB.removeCD(cd);

		shoppingCartEJB.empty();
	}
}
