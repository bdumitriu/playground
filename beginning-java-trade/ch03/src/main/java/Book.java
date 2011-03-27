import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@DiscriminatorValue("B")
@Table(name = "t_book")
public class Book extends Item {

	@Column(name = "book_isbn", length = 20)
	private String isbn;

	@Column(name = "nb_of_page")
	private Integer nbOfPages;

	private Boolean illustrations;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(name = "tag")
	@Column(name = "value")
	private List<String> tags = new ArrayList<String>();

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
