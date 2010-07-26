import static javax.persistence.FetchType.*;

import javax.persistence.*;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Track {

	@Id
	@GeneratedValue
	private Long id;

	private String title;

	private Float duration;

	@Basic(fetch = LAZY)
	@Lob
	private byte[] wav;

	@Basic(optional = true)
	private String description;

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

	public Float getDuration() {
		return duration;
	}

	public void setDuration(Float duration) {
		this.duration = duration;
	}

	public byte[] getWav() {
		return wav;
	}

	public void setWav(byte[] wav) {
		this.wav = wav;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
}
