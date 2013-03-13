package org.ffplanner.def;

import org.joda.time.DateTime;

/**
 * @author Bogdan Dumitriu
 */
public interface ShowingDefinition {

    Long getId();

    Long getVenueId();

    DateTime getDateTime();

    MovieDefinition getMovie();
}
