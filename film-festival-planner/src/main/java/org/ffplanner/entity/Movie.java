package org.ffplanner.entity;

import org.ffplanner.util.DateUtils;

import javax.persistence.*;
import java.io.Serializable;
import java.text.ParseException;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

@Entity
public class Movie implements Serializable {

    private static final long serialVersionUID = 1L;

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
    @OrderBy
    private List<Person> actors;

    @JoinTable(name = "movie_director")
    @ManyToMany(cascade = CascadeType.ALL)
    @OrderBy
    private List<Person> directors;

    @ManyToMany(cascade = CascadeType.ALL)
    @OrderBy
    private List<Country> countries;

    private String imdbId;

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

    public String getFormattedDuration() {
        return new DateUtils().formatHoursAndMinutes(duration);
    }

    public void setDuration(Date duration) {
        this.duration = duration;
    }

    /**
     * @param duration the duration in 01h:47m format
     * @throws ParseException if the {@code duration} is not in the correct format
     */
    public void setDuration(String duration) throws ParseException {
        this.duration = new DateUtils().parseHoursAndMinutes(duration);
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void addActors(Collection<Person> actors) {
        if (this.actors == null) {
            this.actors = new LinkedList<>();
        }
        this.actors.addAll(actors);
    }

    public void addDirectors(Collection<Person> directors) {
        if (this.directors == null) {
            this.directors = new LinkedList<>();
        }
        this.directors.addAll(directors);
    }

    public void addCountries(Collection<Country> countries) {
        if (this.countries == null) {
            this.countries = new LinkedList<>();
        }
        this.countries.addAll(countries);
    }

    public String getImdbId() {
        return imdbId;
    }

    public void setImdbId(String imdbId) {
        this.imdbId = imdbId;
    }

    public int getDurationInMinutes() {
        return new DateUtils().getInMinutes(duration);
    }

    public List<Person> getActors() {
        return actors;
    }

    public List<Person> getDirectors() {
        return directors;
    }

    public List<Country> getCountries() {
        return countries;
    }

    @Override
    public String toString() {
        return "Movie{" +
                "englishTitle='" + englishTitle + '\'' +
                ", originalTitle='" + originalTitle + '\'' +
                ", year='" + year + '\'' +
                ", duration=" + duration +
                ", description='" + description + '\'' +
                ", actors=" + actors +
                ", directors=" + directors +
                ", countries=" + countries +
                ", imdbID=" + imdbId +
                '}';
    }
}
