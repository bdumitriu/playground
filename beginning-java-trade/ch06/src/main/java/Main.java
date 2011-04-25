import javax.ejb.EJB;
import javax.naming.NamingException;

/**
 * @author Bogdan Dumitriu
 */
public class Main {

	@EJB
	private static BookEJBRemote bookEjb;

	public static void main(String[] args) throws NamingException {
		final Book book = new Book();
		book.setTitle("The Game of Thrones");
		book.setPrice(19.99F);
		book.setDescription("Fantasy book by GRRM");
		book.setIsbn("978-0553573404");
		book.setNbOfPages(831);

		// use embedded container
//		final EJBContainer ejbContainer = EJBContainer.createEJBContainer();
//		final Context context = ejbContainer.getContext();
//		bookEjb = (BookEJB) context.lookup("java:global/BookEJB");

		// use JNDI lookup instead of dependency injection
//		final Context context = new InitialContext();
//		bookEjb = (BookEJB) context.lookup("java:global/chapter06/BookEJB");

		bookEjb.createBook(book);
		System.out.println("### book created: " + book);

		book.setTitle("The Game of Thrones (A Song of Ice and Fire, Volume 1)");
		bookEjb.updateBook(book);
		System.out.println("### book updated: " + book);

		bookEjb.deleteBook(book);
		System.out.println("### book deleted");
	}
}
