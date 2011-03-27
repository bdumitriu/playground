import javax.persistence.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@DiscriminatorValue("C")
public class CD extends Item {

	private String musicCompany;

	private Integer numberOfCds;

	private Float totalDuration;

	private String gender;

	@Lob
	private byte[] cover;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(name = "track")
	@MapKeyColumn(name = "position")
	@Column(name = "title")
	private Map<Integer, String> tracks = new HashMap<Integer, String>();

	@ManyToMany(mappedBy = "appearsOn")
	private List<Artist> createdBy;

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

	public byte[] getCover() {
		return cover;
	}

	public void setCover(byte[] cover) {
		this.cover = cover;
	}

	public void putTrack(Integer position, String title) {
		tracks.put(position, title);
	}

	public void removeTrack(Integer position) {
		tracks.remove(position);
	}

	public Map<Integer, String> getTracks() {
		return tracks;
	}

	public List<Artist> getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(List<Artist> createdBy) {
		this.createdBy = createdBy;
	}
}
