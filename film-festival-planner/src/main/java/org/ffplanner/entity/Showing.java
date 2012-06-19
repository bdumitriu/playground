/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import static java.util.Calendar.HOUR_OF_DAY;
import static java.util.Calendar.MINUTE;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Showing implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final SimpleDateFormat DAY_FORMAT = new SimpleDateFormat("d MMM yyyy");

    private static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat("H:m");

    @GeneratedValue
    @Id
    private Long id;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateAndTime;

    @ManyToOne(cascade = CascadeType.ALL)
    private Venue venue;

    @ManyToOne
    private MovieBundle movieBundle;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Date getDateAndTime() {
        return dateAndTime;
    }

    public void setDateAndTime(Date dateAndTime) {
        this.dateAndTime = dateAndTime;
    }

    public int getHour() {
        final Calendar calendar = new GregorianCalendar();
        calendar.setTime(dateAndTime);
        return calendar.get(HOUR_OF_DAY);
    }

    public int getMinute() {
        final Calendar calendar = new GregorianCalendar();
        calendar.setTime(dateAndTime);
        return calendar.get(Calendar.MINUTE);
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

    public MovieBundle getMovieBundle() {
        return movieBundle;
    }

    public void setMovieBundle(MovieBundle movieBundle) {
        this.movieBundle = movieBundle;
    }
}
