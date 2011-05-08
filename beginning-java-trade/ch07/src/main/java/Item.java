import javax.persistence.*;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@DiscriminatorColumn(name = "type", discriminatorType = DiscriminatorType.CHAR)
public abstract class Item implements Serializable {

	@Id
	@GeneratedValue
	protected Long id;

	@Column(nullable = false)
	protected String title;

	@Column(nullable = false)
	protected Float price;

	@Column(length = 2000)
	protected String description;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Float getPrice() {
		return price;
	}

	public void setPrice(Float price) {
		this.price = price;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
}
