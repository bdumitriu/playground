import javax.ejb.Remote;

/**
 * @author Bogdan Dumitriu
 */
@Remote
public interface ItemConverterRemote {

	Float getExchangeRate();
}
