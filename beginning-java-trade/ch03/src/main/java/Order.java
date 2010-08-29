import javax.persistence.*;
import java.util.Date;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Order {

	@Id
	@GeneratedValue
	private Long id;

	@Temporal(TemporalType.TIMESTAMP)
	private Date creationDate;

	@OneToMany(fetch = FetchType.LAZY)
	@JoinTable(name = "jnd_ord_line",
			joinColumns = @JoinColumn(name = "order_fk"),
			inverseJoinColumns = @JoinColumn(name = "order_line_fk"))
	private List<OrderLine> orderLines;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Date getCreationDate() {
		return creationDate;
	}

	public void setCreationDate(Date creationDate) {
		this.creationDate = creationDate;
	}

	public List<OrderLine> getOrderLines() {
		return orderLines;
	}

	public void setOrderLines(List<OrderLine> orderLines) {
		this.orderLines = orderLines;
	}
}
