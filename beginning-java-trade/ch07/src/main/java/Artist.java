import javax.persistence.*;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Artist {

	@Id
	@GeneratedValue
	private Long id;

	private String firstName;

	private String lastName;

	@ManyToMany
	private List<CD> appearsOn;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public List<CD> getAppearsOn() {
		return appearsOn;
	}

	public void setAppearsOn(List<CD> appearsOn) {
		this.appearsOn = appearsOn;
	}
}
