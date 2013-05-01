package org.ffplanner.entity;

import org.ffplanner.util.DateUtils;

import javax.persistence.*;
import java.io.Serializable;
import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class MovieBundle implements Serializable {

    private static final long serialVersionUID = 1L;

    @GeneratedValue
    @Id
    private Long id;

    private String englishTitle;

    private String originalTitle;

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(name = "moviebundle_movie",
            joinColumns = {@JoinColumn(name = "movieBundle_id")},
            inverseJoinColumns = {@JoinColumn(name = "movie_id")}
    )
    @OrderBy
    private List<Movie> movies = new LinkedList<>();

    @Transient
    private boolean lazyFieldsLoaded;

    public synchronized void loadLazyFields() {
        if (!lazyFieldsLoaded) {
            for (Movie movie : movies) {
                movie.loadLazyFields();
            }
            lazyFieldsLoaded = true;
        }
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getEnglishTitle() {
        return movies.size() == 1 ? movies.iterator().next().getEnglishTitle() : englishTitle;
    }

    public void setEnglishTitle(String englishTitle) {
        this.englishTitle = englishTitle;
    }

    public String getOriginalTitle() {
        return movies.size() == 1 ? movies.iterator().next().getOriginalTitle() : originalTitle;
    }

    public void setOriginalTitle(String originalTitle) {
        this.originalTitle = originalTitle;
    }

    public boolean hasSingleMovie() {
        return movies.size() == 1;
    }

    public Movie getSingleMovie() {
        return movies.size() == 1 ? movies.get(0) : null;
    }

    public void addMovies(Collection<Movie> movies) {
        if (this.movies == null) {
            this.movies = new LinkedList<>();
        }
        this.movies.addAll(movies);
    }

    public List<Movie> getMovies() {
        return movies;
    }

    public String getFormattedDuration() {
        return DateUtils.formatHoursAndMinutes(getDurationInMinutes());
    }

    public int getDurationInMinutes() {
        int totalDuration = 0;
        for (Movie movie : movies) {
            totalDuration += movie.getDurationInMinutes();
        }
        return totalDuration;
    }
}
