/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import java.text.ParseException;

import org.ffplanner.entity.MovieBundle;
import org.ffplanner.entity.Showing;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class ShowingEJB {

	@PersistenceContext(unitName = "ffp")
	private EntityManager entityManager;

	@Inject
	private VenueEJB venueEJB;

	/**
	 * @param movieBundle
	 *            the movie bundle being shown
	 * @param day
	 *            expected in the format "9 Jun 2011"
	 * @param time
	 *            expected in the format "16:00"
	 * @param venueName
	 *            where the showing takes places
	 * @throws ParseException if the {@code duration} is not in the correct format
	 */
	public void addShowing(MovieBundle movieBundle, String day, String time, String venueName) throws ParseException {
		final Showing showing = new Showing();
		showing.setDateAndTime(day, time);
		showing.setVenue(venueEJB.getVenue(venueName));
		showing.setMovieBundle(movieBundle);
		entityManager.persist(showing);
	}
}
