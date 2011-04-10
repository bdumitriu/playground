import javax.persistence.PostPersist;
import javax.persistence.PrePersist;

/**
 * @author Bogdan Dumitriu
 */
public class DebugListener {

	@PrePersist
	void prePersist(Object object) {
		System.out.println("prePersist: " + object);
	}

	@PostPersist
	void postPersist(Object object) {
		System.out.println("postPersist: " + object);
	}
}
