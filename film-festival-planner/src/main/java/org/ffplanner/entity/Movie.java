package org.ffplanner.entity;

import javax.persistence.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

@Entity
public class Movie {

	private static final SimpleDateFormat DURATION_FORMAT = new SimpleDateFormat("H'h':m'm'");

	@GeneratedValue
	@Id
	private Long id;

	private String englishTitle;

	private String originalTitle;

	private String year;

	@Temporal(TemporalType.TIME)
	private Date duration;

	@Column(length = 2000)
	private String description;

	@JoinTable(name = "movie_actor")
	@ManyToMany(cascade = CascadeType.ALL)
	private List<Person> actors;

	@JoinTable(name = "movie_director")
	@ManyToMany(cascade = CascadeType.ALL)
	private List<Person> directors;

	@ManyToMany(cascade = CascadeType.ALL)
	private List<Country> countries;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getEnglishTitle() {
		return englishTitle;
	}

	public void setEnglishTitle(String englishTitle) {
		this.englishTitle = englishTitle;
	}

	public String getOriginalTitle() {
		return originalTitle;
	}

	public void setOriginalTitle(String originalTitle) {
		this.originalTitle = originalTitle;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public Date getDuration() {
		return duration;
	}

	public void setDuration(Date duration) {
		this.duration = duration;
	}

	/**
	 * @param duration
	 *            the duration in 01h:47m format
	 * @throws ParseException if the {@code duration} is not in the correct format
	 */
	public void setDuration(String duration) throws ParseException {
		this.duration = DURATION_FORMAT.parse(duration);
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void addActors(Collection<Person> actors) {
		if (this.actors == null) {
			this.actors = new ArrayList<>();
		}
		this.actors.addAll(actors);
	}

	public void addDirectors(Collection<Person> directors) {
		if (this.directors == null) {
			this.directors = new ArrayList<>();
		}
		this.directors.addAll(directors);
	}

	public void addCountries(Collection<Country> countries) {
		if (this.countries == null) {
			this.countries = new ArrayList<>();
		}
		this.countries.addAll(countries);
	}
}
