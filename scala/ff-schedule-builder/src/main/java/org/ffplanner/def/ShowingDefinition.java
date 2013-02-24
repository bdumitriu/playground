package org.ffplanner.def;

import org.joda.time.DateTime;

/**
 * @author Bogdan Dumitriu
 */
public interface ShowingDefinition {

    Long getId();

    DateTime getDateTime();

    MovieDefinition getMovie();
}
