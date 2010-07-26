import javax.persistence.Embeddable;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Embeddable
public class NewsId implements Serializable {

	private String title;
	
	private String language;

	public NewsId() {
	}

	public NewsId(String title, String language) {
		this.title = title;
		this.language = language;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}

		final NewsId newsId = (NewsId) o;

		if (language != null ? !language.equals(newsId.language) : newsId.language != null) {
			return false;
		}
		if (title != null ? !title.equals(newsId.title) : newsId.title != null) {
			return false;
		}

		return true;
	}

	@Override
	public int hashCode() {
		int result = title != null ? title.hashCode() : 0;
		result = 31 * result + (language != null ? language.hashCode() : 0);
		return result;
	}
}
