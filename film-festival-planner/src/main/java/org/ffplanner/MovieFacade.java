package org.ffplanner;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import org.ffplanner.entity.Movie;

/**
 *
 *
 * @author Bogdan Dumitriu
 */
@Stateless
public class MovieFacade extends AbstractFacade<Movie> {

	@PersistenceContext(unitName = "ffp")
	private EntityManager em;

	protected EntityManager getEntityManager() {
		return em;
	}

	public MovieFacade() {
		super(Movie.class);
	}

}
