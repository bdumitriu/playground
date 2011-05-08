import javax.ejb.embeddable.EJBContainer;
import javax.naming.Context;
import javax.naming.NamingException;
import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.junit.*;

/**
 * @author Bogdan Dumitriu
 */
public class ItemTest {

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
	public void testLocalInterface() throws NamingException {
		// java:global[/<appName>]/<moduleName>/<beanName>[!<fullyQualifiedInterfaceName>]
		final ItemLocal itemLocal = (ItemLocal) context.lookup("java:global/ch07Test/classes/ItemEJB!ItemLocal");

		Assert.assertTrue(itemLocal.findBooks().isEmpty());
		Assert.assertTrue(itemLocal.findCDs().isEmpty());
	}

	@Test(expected = NamingException.class)
	public void testRemoteInterfaceCannotBeAccessedFromTheSameJVM() throws NamingException {
		// java:global[/<appName>]/<moduleName>/<beanName>[!<fullyQualifiedInterfaceName>]
		context.lookup("java:global/ch07Test/classes/ItemEJB!ItemRemote");
	}

	@Test
	public void testNoInterfaceView() throws NamingException {
		// java:global[/<appName>]/<moduleName>/<beanName>[!<fullyQualifiedInterfaceName>]
		final ItemEJB itemEJB = (ItemEJB) context.lookup("java:global/ch07Test/classes/ItemEJB!ItemEJB");

		final Book book = new Book();
		book.setTitle("The Game of Thrones");
		book.setPrice(19.99F);
		book.setDescription("Fantasy book by GRRM");
		book.setIsbn("978-0553573404");
		book.setNbOfPages(831);

		Assert.assertNull(book.getId());
		itemEJB.createBook(book);
		Assert.assertNotNull(book.getId());

		Assert.assertEquals(1, itemEJB.findBooks().size());
		Assert.assertTrue(itemEJB.findCDs().isEmpty());

		itemEJB.removeBook(book);
	}

	@Test
	public void testSessionContext() throws NamingException {
		// java:global[/<appName>]/<moduleName>/<beanName>[!<fullyQualifiedInterfaceName>]
		final ItemEJB itemEJB = (ItemEJB) context.lookup("java:global/ch07Test/classes/ItemEJB!ItemEJB");

		final Book book = new Book();
		book.setTitle("The Game of Thrones");
		book.setPrice(19.99F);
		book.setDescription("Fantasy book by GRRM");
		book.setIsbn("978-0553573404");
		book.setNbOfPages(831);

		itemEJB.alwaysFailingCreateBook(book);
		Assert.assertTrue(itemEJB.findBooks().isEmpty());
	}
}
