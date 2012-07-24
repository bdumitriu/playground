/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.MovieBundle;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Showing_;

import javax.ejb.EJB;
import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.text.ParseException;
import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class ShowingEJB {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @EJB
    private VenueEJB venueEJB;

    /**
     * @param movieBundle
     *         the movie bundle being shown
     * @param day
     *         expected in the format "9 Jun 2011"
     * @param time
     *         expected in the format "16:00"
     * @param venueName
     *         where the showing takes places
     * @throws ParseException
     *         if the {@code duration} is not in the correct format
     */
    public void addShowing(MovieBundle movieBundle, String day, String time, String venueName) throws ParseException {
        final Showing showing = new Showing();
        showing.setDateAndTime(day, time);
        showing.setVenue(venueEJB.getVenue(venueName));
        showing.setMovieBundle(movieBundle);
        entityManager.persist(showing);
    }

    /**
     * @param movieBundle
     *         the movie bundle being shown
     * @param day
     *         expected in the format "9 Jun 2011"
     * @param time
     *         expected in the format "16:00"
     * @param venueName
     *         where the showing takes places
     * @throws ParseException
     *         if the {@code duration} is not in the correct format
     */
    public void addShowing(MovieBundle movieBundle, Date day, String time, String venueName) throws ParseException {
        final Showing showing = new Showing();
        showing.setDateAndTime(day, time);
        showing.setVenue(venueEJB.getVenue(venueName));
        showing.setMovieBundle(movieBundle);
        entityManager.persist(showing);
    }

    public Collection<Showing> getShowingsFor(Date day) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Showing> query = criteriaBuilder.createQuery(Showing.class);
        final Root<Showing> root = query.from(Showing.class);
        query.where(criteriaBuilder.greaterThanOrEqualTo(root.get(Showing_.dateAndTime), day),
                criteriaBuilder.lessThanOrEqualTo(root.get(Showing_.dateAndTime), getDayAfter(day)));
        query.orderBy(criteriaBuilder.asc(root.get(Showing_.venue)), criteriaBuilder.asc(root.get(
                Showing_.dateAndTime)));
        final TypedQuery<Showing> showings = entityManager.createQuery(query);
        final List<Showing> xList = showings.getResultList();
        for (Showing showing : xList) {
            showing.getMovieBundle().getMovies().size();
        }
        return xList;
    }

    private Date getDayAfter(Date day) {
        final Calendar calendar = new GregorianCalendar();
        calendar.setTime(day);
        calendar.roll(Calendar.DAY_OF_MONTH, true);
        return calendar.getTime();
    }
}
