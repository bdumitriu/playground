import javax.persistence.*;
import java.util.Date;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Comment {

	@Id
	@GeneratedValue
	private Long id;

	private String nickname;

	private String content;

	private Integer note;

	@Column(name = "posted_at")
	@Temporal(TemporalType.TIMESTAMP)
	private Date postedAt;
}
