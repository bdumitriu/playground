package org.ffplanner.def;

import java.util.List;

import static java.util.Collections.emptyList;

/**
 * @author Bogdan Dumitriu
 */
public interface FestivalProgrammeDefinition {

    FestivalProgrammeDefinition EMPTY = new Empty();

    class Empty implements FestivalProgrammeDefinition {

        private Empty() {}

        @Override
        public List<ShowingDefinition> getShowings() {
            return emptyList();
        }
    }

    List<ShowingDefinition> getShowings();
}
