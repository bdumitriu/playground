import javax.annotation.Resource;
import javax.ejb.LocalBean;
import javax.ejb.Stateless;

/**
 * Demos access of resource configurable in ejb-jar.xml.
 *
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class ItemConverterEJB implements ItemConverterRemote {

	@Resource(name = "exchangeRateEntry")
	private Float exchangeRate;

	public Float getExchangeRate() {
		return exchangeRate;
	}

	public Item convertPrice(Item item) {
		final Float price = item.getPrice();
		if (price != null) {
			item.setPrice(price * exchangeRate);
		}
		return item;
	}
}
