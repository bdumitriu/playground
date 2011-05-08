import javax.ejb.*;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @author Bogdan Dumitriu
 */
@Singleton
@Startup
@DependsOn({"ZipCodeEJB", "CountryCodeEJB"})
@ConcurrencyManagement(ConcurrencyManagementType.CONTAINER)
@Lock(LockType.WRITE)
@AccessTimeout(value = 2, unit = TimeUnit.SECONDS)
public class CacheEJB {

	private Map<Long, Object> cache = new HashMap<Long, Object>();

	public void addToCache(Long id, Object object) {
		if (!cache.containsKey(id)) {
			cache.put(id, object);
		}
	}

	public void removeFromCache(Long id) {
		cache.remove(id);
	}

	@Lock(LockType.READ)
	public Object getFromCache(Long id) {
		return cache.get(id);
	}
}
