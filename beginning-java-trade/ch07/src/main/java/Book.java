import javax.persistence.*;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@DiscriminatorValue("B")
@NamedQuery(name = Book.FIND_ALL_BOOKS, query = "select b from Book b")
public class Book extends Item implements Serializable {

	public final static String FIND_ALL_BOOKS = "Book.findAllBooks";

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

	public Boolean getIllustrations() {
		return illustrations;
	}

	public void setIllustrations(Boolean illustrations) {
		this.illustrations = illustrations;
	}
}
