package org.ffplanner.bean.programme;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.FestivalEditionSection;
import org.ffplanner.entity.MovieBundleInFestival;
import org.ffplanner.entity.Showing;
import org.ffplanner.util.DateUtils;
import org.joda.time.Interval;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class FestivalEditionProgramme {

    private final FestivalEdition festivalEdition;

    private final Map<Long, MovieBundleInFestival> movies;

    private final Map<Long, Showing> showings;

    private final ListMultimap<MovieBundleInFestival, Showing> movieShowings;

    private final Map<Interval, DayProgramme> dayProgrammes;

    private final List<FestivalEditionSection> sectionMovies;

    public FestivalEditionProgramme(FestivalEdition festivalEdition, List<Showing> festivalShowings) {
        this.festivalEdition = festivalEdition;
        this.movies = createMovies(festivalShowings);
        this.showings = createShowings(festivalShowings);
        this.movieShowings = createMovieShowings(festivalShowings);
        this.dayProgrammes = new DayProgrammesLoader(festivalShowings).getDayProgrammes();
        this.sectionMovies = festivalEdition.getSections();
    }

    private static Map<Long, MovieBundleInFestival> createMovies(Iterable<Showing> festivalShowings) {
        final Map<Long, MovieBundleInFestival> movies = new HashMap<>();
        for (Showing showing : festivalShowings) {
            final MovieBundleInFestival movieBundleInFestival = showing.getMovieBundleInFestival();
            movies.put(movieBundleInFestival.getId(), movieBundleInFestival);
        }
        return movies;
    }

    private static Map<Long, Showing> createShowings(Iterable<Showing> festivalShowings) {
        final Map<Long, Showing> showings = new HashMap<>();
        for (Showing showing : festivalShowings) {
            showings.put(showing.getId(), showing);
        }
        return showings;
    }

    private static ListMultimap<MovieBundleInFestival, Showing> createMovieShowings(Iterable<Showing> festivalShowings) {
        final ListMultimap<MovieBundleInFestival, Showing> multimap = ArrayListMultimap.create();
        for (Showing showing : festivalShowings) {
            multimap.put(showing.getMovieBundleInFestival(), showing);
        }
        return multimap;
    }

    public FestivalEdition getFestivalEdition() {
        return festivalEdition;
    }

    public DayProgramme getDayProgramme(Date date) {
        return getDayProgramme(DateUtils.getDayInterval(date));
    }

    public DayProgramme getDayProgramme(Interval day) {
        final DayProgramme dayProgramme = dayProgrammes.get(day);
        return dayProgramme == null ? DayProgramme.getEmptyProgramme() : dayProgramme;
    }

    public List<FestivalEditionSection> getSections() {
        return sectionMovies;
    }

    public MovieBundleInFestival getMovie(Long movieBundleInFestivalId) {
        return movies.get(movieBundleInFestivalId);
    }

    public Showing getShowingFor(Long showingId) {
        return showings.get(showingId);
    }

    public List<Showing> getShowingsFor(Long movieBundleInFestivalId) {
        return getShowingsFor(getMovie(movieBundleInFestivalId));
    }

    public List<Showing> getShowingsFor(MovieBundleInFestival movieBundle) {
        return movieShowings.get(movieBundle);
    }

    public List<Showing> getShowingsForSameMovieAs(Showing showing) {
        final MovieBundleInFestival movieBundleInFestival = showing.getMovieBundleInFestival();
        return getShowingsFor(movieBundleInFestival);
    }
}
