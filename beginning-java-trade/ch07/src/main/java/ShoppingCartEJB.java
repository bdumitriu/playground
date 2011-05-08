import javax.annotation.Resource;
import javax.ejb.*;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang.StringUtils;

/**
 * @author Bogdan Dumitriu
 */
@Stateful
@StatefulTimeout(value = 20, unit = TimeUnit.SECONDS)
@LocalBean
public class ShoppingCartEJB implements ShoppingCartRemote {

	private Set<Item> cartItems = new HashSet<Item>();

	@EJB
	private ItemConverterEJB itemConverter;

	@Resource(name = "currencyEntry")
	private String currency;

	public void addItem(Item item) {
		cartItems.add(itemConverter.convertPrice(item));
	}

	public void removeItem(Item item) {
		cartItems.remove(item);
	}

	public Float getTotal() {
		float total = 0;
		for (Item cartItem : cartItems) {
			total += cartItem.getPrice();
		}
		return total;
	}

	public void checkout() {
		printReceipt();
		cartItems.clear();
	}

	private void printReceipt() {
		final int width = 80;
		printHeader(width);
		for (Item cartItem : cartItems) {
			printTitleAndPrice(cartItem.getTitle(), cartItem.getPrice(), width);
		}
		printSeparator(width);
		printTitleAndPrice("Total:", getTotal(), width);
		printFooter(width);
	}

	private void printHeader(int width) {
		printText("START RECEIPT", width);
	}

	private void printFooter(int width) {
		printText("END RECEIPT  ", width);
	}

	private void printText(String text, int width) {
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("### --- ");
		stringBuilder.append(text);
		stringBuilder.append(" ");
		stringBuilder.append(StringUtils.repeat("-", width - 13 - text.length()));
		stringBuilder.append(" ###");
		System.out.println(stringBuilder.toString());
	}

	private void printTitleAndPrice(String title, Float price, int width) {
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("### ");
		stringBuilder.append(title);
		final int spacesInsertPosition = stringBuilder.length();
		stringBuilder.append(currency);
		stringBuilder.append(String.format("%.2f", price));
		stringBuilder.append(" ###");
		stringBuilder.insert(spacesInsertPosition, StringUtils.repeat(" ", width - stringBuilder.length()));
		System.out.println(stringBuilder.toString());
	}

	private void printSeparator(int width) {
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(StringUtils.rightPad("### ", width - 4, '-'));
		stringBuilder.append(" ###");
		System.out.println(stringBuilder.toString());
	}

	@Remove
	public void empty() {
		cartItems.clear();
	}
}
