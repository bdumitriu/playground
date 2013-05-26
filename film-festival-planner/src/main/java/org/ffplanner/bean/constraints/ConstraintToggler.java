package org.ffplanner.bean.constraints;

import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;

import javax.persistence.EntityManager;
import java.util.Collection;

/**
 * @author Bogdan Dumitriu
 */
public abstract class ConstraintToggler extends ConstraintChanger {

    protected ConstraintToggler(EntityManager entityManager,
            Showing showing, Collection<Showing> allShowings, UserSchedule userSchedule) {
        super(entityManager, showing, allShowings, userSchedule);
    }

    protected ConstraintToggler(ConstraintChanger constraintChanger) {
        super(constraintChanger);
    }

    public abstract void change();
}
