package ass1.model;

import java.io.Serializable;

public class Publisher implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private String website;

	public Publisher(String name) {
		this(name, null);
	}

	public Publisher(String name, String website) {
		this.name = name;
		this.website = website;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getWebsite() {
		return website;
	}

	public void setWebsite(String website) {
		this.website = website;
	}
}
