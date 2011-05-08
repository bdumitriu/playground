import javax.ejb.Remote;

/**
 * @author Bogdan Dumitriu
 */
@Remote
public interface ShoppingCartRemote {

	void addItem(Item item);

	void removeItem(Item item);

	Float getTotal();

	void checkout();

	void empty();
}
