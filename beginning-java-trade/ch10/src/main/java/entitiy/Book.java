package entitiy;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Book implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long id;

	@Column(nullable = false)
	private String title;

	private String price;

	@Column(length = 2000)
	private String description;

	private String isbn;

	private Integer numberOfPages;

	private Boolean illustration;

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

	public String getPrice() {
		return price;
	}

	public void setPrice(String price) {
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

	public Integer getNumberOfPages() {
		return numberOfPages;
	}

	public void setNumberOfPages(Integer numberOfPages) {
		this.numberOfPages = numberOfPages;
	}

	public Boolean getIllustration() {
		return illustration;
	}

	public void setIllustration(Boolean illustration) {
		this.illustration = illustration;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final Book other = (Book) obj;
		if (this.id != other.id && (this.id == null || !this.id.equals(other.id))) {
			return false;
		}
		if ((this.title == null) ? (other.title != null) : !this.title.equals(other.title)) {
			return false;
		}
		if ((this.price == null) ? (other.price != null) : !this.price.equals(other.price)) {
			return false;
		}
		if ((this.description == null) ? (other.description != null) : !this.description.equals(other.description)) {
			return false;
		}
		if ((this.isbn == null) ? (other.isbn != null) : !this.isbn.equals(other.isbn)) {
			return false;
		}
		if (this.numberOfPages != other.numberOfPages && (this.numberOfPages == null
				|| !this.numberOfPages.equals(other.numberOfPages))) {
			return false;
		}
		if (this.illustration != other.illustration && (this.illustration == null || !this.illustration.equals(
				other.illustration))) {
			return false;
		}
		return true;
	}

	@Override
	public int hashCode() {
		int hash = 7;
		hash = 37 * hash + (this.id != null ? this.id.hashCode() : 0);
		hash = 37 * hash + (this.title != null ? this.title.hashCode() : 0);
		hash = 37 * hash + (this.price != null ? this.price.hashCode() : 0);
		hash = 37 * hash + (this.description != null ? this.description.hashCode() : 0);
		hash = 37 * hash + (this.isbn != null ? this.isbn.hashCode() : 0);
		hash = 37 * hash + (this.numberOfPages != null ? this.numberOfPages.hashCode() : 0);
		hash = 37 * hash + (this.illustration != null ? this.illustration.hashCode() : 0);
		return hash;
	}

	@Override
	public String toString() {
		return "Book{" + "id=" + id + ", title=" + title + ", price=" + price + ", description=" + description
				+ ", isbn=" + isbn + ", numberOfPages=" + numberOfPages + ", illustration=" + illustration + '}';
	}
}
