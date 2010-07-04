import javax.persistence.*;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@NamedQueries({
		@NamedQuery(name = "findAllBooks", query = "SELECT b FROM Book b"),
		@NamedQuery(name = "findBookByTitle", query = "SELECT b FROM Book b WHERE b.title = 'GEB'")
})
public class Book {

	@Id
	@GeneratedValue
	private Long id;

	@Column(nullable = false)
	private String title;

	private Float price;

	@Column(length = 2000)
	private String description;

	private String isbn;

	private Integer nbOfPages;

	private Boolean illustrations;

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
}
