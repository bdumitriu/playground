import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class News {

	@EmbeddedId
	private NewsId id;

	private String content;

	@OneToMany
	@OrderBy("postedAt desc")
	private List<Comment> comments;

	public NewsId getId() {
		return id;
	}

	public void setId(NewsId id) {
		this.id = id;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}
}
