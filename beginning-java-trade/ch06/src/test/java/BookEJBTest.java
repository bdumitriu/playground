import javax.ejb.embeddable.EJBContainer;
import javax.naming.Context;
import javax.naming.NamingException;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.Assert;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Bogdan Dumitriu
 */
public class BookEJBTest {

	private static EJBContainer ejbContainer;

	private static Context context;

	@BeforeClass
	public static void initContainer() {
		final Map<String, Object> properties = new HashMap<String, Object>();
		properties.put(EJBContainer.MODULES, new File("target/classes"));
		// this makes embedded glassfish use the domain.xml that contains the Chapter06Pool resource configuration
		properties.put("org.glassfish.ejb.embedded.glassfish.installation.root", "./target/test-classes/glassfish");
		ejbContainer = EJBContainer.createEJBContainer(properties);
		context = ejbContainer.getContext();
	}

	@AfterClass
	public static void closeContainer() {
		ejbContainer.close();
	}

	@Test
	public void createBook() throws NamingException {
		final Book book = new Book();
		book.setTitle("The Game of Thrones");
		book.setPrice(19.99F);
		book.setDescription("Fantasy book by GRRM");
		book.setIsbn("978-0553573404");
		book.setNbOfPages(831);

		final BookEJB bookEjb = (BookEJB) context.lookup("java:global/classes/BookEJB!BookEJB");

		Assert.assertNull(book.getId());
		final Book persistedBook = bookEjb.createBook(book);
		Assert.assertNotNull(persistedBook.getId());

		final List<Book> books = bookEjb.findBooks();
		Assert.assertEquals(1, books.size());

		bookEjb.deleteBook(book);
	}
}
