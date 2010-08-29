import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "t_book")
public class Book {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long id;

	@Column(name = "book_title", nullable = false, updatable = false)
	private String title;

	private Float price;

	@Column(length = 2000)
	private String description;

	private String isbn;

	@Column(name = "nb_of_page", nullable = false)
	private Integer nbOfPages;

	private Boolean illustrations;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(name = "tag")
	@Column(name = "value")
	private List<String> tags = new ArrayList<String>();

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

	public String getIsbn() {
		return isbn;
	}

	public void setIsbn(String isbn) {
		this.isbn = isbn;
	}

	public Integer getNbOfPages() {
		return nbOfPages;
	}

	public void setNbOfPages(Integer nbOfPages) {
		this.nbOfPages = nbOfPages;
	}

	public Boolean isIllustrations() {
		return illustrations;
	}

	public void setIllustrations(Boolean illustrations) {
		this.illustrations = illustrations;
	}

	public void addTag(String tag) {
		tags.add(tag);
	}

	public void removeTag(String tag) {
		tags.remove(tag);
	}

	public void clearTags() {
		tags.clear();
	}

	public List<String> getTags() {
		return tags;
	}
}
