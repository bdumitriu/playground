import javax.persistence.*;
import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@DiscriminatorValue("C")
@NamedQuery(name = CD.FIND_ALL_CDS, query = "select c from CD c")
public class CD extends Item implements Serializable {

	public final static String FIND_ALL_CDS = "CD.findAllCDs";

	private String musicCompany;

	private Integer numberOfCds;

	private Float totalDuration;

	private String gender;

	@Lob
	private byte[] cover;

	@ElementCollection(fetch = FetchType.LAZY)
	private Map<Integer, String> tracks = new HashMap<Integer, String>();

	@ManyToMany(mappedBy = "appearsOn")
	private List<Artist> createdBy;

	public String getMusicCompany() {
		return musicCompany;
	}

	public void setMusicCompany(String musicCompany) {
		this.musicCompany = musicCompany;
	}

	public Integer getNumberOfCds() {
		return numberOfCds;
	}

	public void setNumberOfCds(Integer numberOfCds) {
		this.numberOfCds = numberOfCds;
	}

	public Float getTotalDuration() {
		return totalDuration;
	}

	public void setTotalDuration(Float totalDuration) {
		this.totalDuration = totalDuration;
	}

	public String getGender() {
		return gender;
	}

	public void setGender(String gender) {
		this.gender = gender;
	}

	public byte[] getCover() {
		return cover;
	}

	public void setCover(byte[] cover) {
		this.cover = cover;
	}

	public Map<Integer, String> getTracks() {
		return tracks;
	}

	public void setTracks(Map<Integer, String> tracks) {
		this.tracks = tracks;
	}

	public List<Artist> getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(List<Artist> createdBy) {
		this.createdBy = createdBy;
	}
}
