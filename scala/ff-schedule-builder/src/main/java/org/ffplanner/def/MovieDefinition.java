package org.ffplanner.def;

import org.joda.time.Period;

/**
 * @author Bogdan Dumitriu
 */
public interface MovieDefinition {

    Long getId();

    /**
     * @return the
     */
    Period getDuration();
}
