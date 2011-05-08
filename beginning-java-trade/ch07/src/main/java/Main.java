import javax.ejb.EJB;

/**
 * @author Bogdan Dumitriu
 */
public class Main {

	@EJB
	private static ItemRemote item;

	public static void main(String[] args) {
		final Book book = new Book();
		book.setTitle("The Game of Thrones");
		book.setPrice(19.99F);
		book.setDescription("Fantasy book by GRRM");
		book.setIsbn("978-0553573404");
		book.setNbOfPages(831);

		final CD cd = new CD();
		cd.setTitle("Pulp Fiction Soundtrack");
		cd.setPrice(9.99F);

		item.createBook(book);
		item.createCD(cd);
		for (Book b : item.findBooks()) {
			System.out.println(b.getTitle());
		}
		for (CD c : item.findCDs()) {
			System.out.println(c.getTitle());
		}
	}
}
