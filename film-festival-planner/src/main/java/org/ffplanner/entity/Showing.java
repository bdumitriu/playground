package org.ffplanner.entity;

import org.ffplanner.def.MovieDefinition;
import org.ffplanner.def.ShowingDefinition;
import org.ffplanner.util.DateUtils;
import org.joda.time.DateTime;
import org.joda.time.Interval;

import javax.persistence.*;
import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.Objects;

import static java.util.Calendar.HOUR_OF_DAY;
import static java.util.Calendar.MINUTE;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "showing")
public class Showing implements Serializable, Comparable<Showing>, ShowingDefinition {

    private static final long serialVersionUID = 1L;

    private static final SimpleDateFormat DAY_FORMAT = new SimpleDateFormat("d MMM yyyy");

    private static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat("H:m");

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateAndTime;

    @Column(name = "venue_id", insertable = false, updatable = false)
    private Long venueId;

    @ManyToOne
    @JoinColumn(name = "venue_id")
    private Venue venue;

    @Column(name = "movieBundleInFestival_id", insertable = false, updatable = false)
    private Long movieBundleInFestivalId;

    @ManyToOne
    private MovieBundleInFestival movieBundleInFestival;

    public void loadLazyFields() {
        movieBundleInFestival.loadLazyFields();
    }

    @Override
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public FestivalEdition getFestivalEdition() {
        return movieBundleInFestival.getFestivalEditionSection().getFestivalEdition();
    }

    public Date getDateAndTime() {
        return dateAndTime;
    }

    public void setDateAndTime(Date dateAndTime) {
        this.dateAndTime = dateAndTime;
    }

    @Override
    public DateTime getDateTime() {
        return new DateTime(dateAndTime);
    }

    public Interval getDayInterval() {
        return DateUtils.getDayInterval(getDateAndTime());
    }

    public String getDayOfWeek(Locale locale) {
        return DateUtils.getDayOfWeek(dateAndTime, locale);
    }

    public String getDayMonth(Locale locale) {
        return DateUtils.getDayMonth(dateAndTime, locale);
    }

    public int getHour() {
        return new DateTime(dateAndTime).getHourOfDay();
    }

    public int getMinute() {
        return new DateTime(dateAndTime).getMinuteOfHour();
    }

    /**
     * @param day  expected in the format "9 Jun 2011"
     * @param time expected in the format "16:00"
     * @throws ParseException if the {@code duration} is not in the correct format
     */
    public void setDateAndTime(String day, String time) throws ParseException {
        final Date dayDate = DAY_FORMAT.parse(day);
        setDateAndTime(dayDate, time);
    }

    public void setDateAndTime(Date day, String time) throws ParseException {
        final Date timeDate = TIME_FORMAT.parse(time);
        final GregorianCalendar timeCalendar = new GregorianCalendar();
        timeCalendar.setTime(timeDate);
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(day);
        calendar.set(HOUR_OF_DAY, timeCalendar.get(HOUR_OF_DAY));
        calendar.set(MINUTE, timeCalendar.get(MINUTE));
        dateAndTime = calendar.getTime();
    }

    public Venue getVenue() {
        return venue;
    }

    public void setVenue(Venue venue) {
        this.venue = venue;
    }

    @Override
    public Long getVenueId() {
        return venue.getId();
    }

    @Override
    public MovieDefinition getMovie() {
        return movieBundleInFestival;
    }

    public MovieBundleInFestival getMovieBundleInFestival() {
        return movieBundleInFestival;
    }

    public void setMovieBundleInFestival(MovieBundleInFestival movieBundleInFestival) {
        this.movieBundleInFestival = movieBundleInFestival;
    }

    @Override
    public int hashCode() {
        return Objects.hash(movieBundleInFestivalId, dateAndTime, venueId);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final Showing other = (Showing) obj;
        return Objects.equals(this.movieBundleInFestivalId, other.movieBundleInFestivalId)
                && Objects.equals(this.dateAndTime, other.dateAndTime) && Objects.equals(this.venueId, other.venueId);
    }

    @Override
    public int compareTo(Showing other) {
        if (this.equals(other)) {
            return 0;
        } else {
            return dateAndTime.compareTo(other.dateAndTime);
        }
    }
}
