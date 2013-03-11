package org.ffplanner.controller.constraints;

import org.ffplanner.def.ConstraintDefinition;
import org.ffplanner.def.ScheduleDefinition;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class ConstraintsDefinition implements ScheduleDefinition {

    private Collection<ConstraintDefinition.Showing> showingConstraints;

    private Collection<ConstraintDefinition.Movie> movieConstraints;

    public void initializeFrom(Map<Long, QualifiedConstraint> constraints) {
        this.showingConstraints = new LinkedList<>();
        this.movieConstraints = new LinkedList<>();
        for (QualifiedConstraint qualifiedConstraint : constraints.values()) {
            if (qualifiedConstraint instanceof ConstraintDefinition.Showing) {
                this.showingConstraints.add((ConstraintDefinition.Showing) qualifiedConstraint);
            } else if (qualifiedConstraint instanceof ConstraintDefinition.Movie) {
                this.movieConstraints.add((ConstraintDefinition.Movie) qualifiedConstraint);
            }
        }
    }

    @Override
    public Collection<ConstraintDefinition.Showing> getShowingConstraints() {
        return showingConstraints;
    }

    @Override
    public Collection<ConstraintDefinition.Movie> getMovieConstraints() {
        return movieConstraints;
    }
}
