import javax.persistence.*;

/**
 * This class is kept only for reference purposes. Nobody uses it.
 *
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "t_address")
@SecondaryTables({
	@SecondaryTable(name = "t_city"),
	@SecondaryTable(name = "t_country")
})
public class AddressTableSplitExample {

	@Id
	@GeneratedValue
	private Long id;

	private String street1;

	private String street2;

	@Column(table = "t_city")
	private String city;

	@Column(table = "t_city")
	private String state;

	@Column(table = "t_city")
	private String zipcode;

	@Column(table = "t_country")
	private String country;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getStreet1() {
		return street1;
	}

	public void setStreet1(String street1) {
		this.street1 = street1;
	}

	public String getStreet2() {
		return street2;
	}

	public void setStreet2(String street2) {
		this.street2 = street2;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getZipcode() {
		return zipcode;
	}

	public void setZipcode(String zipcode) {
		this.zipcode = zipcode;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}
}
