package org.ffplanner.bean;

import org.ffplanner.bean.constraints.AnyConstraintToggler;
import org.ffplanner.bean.constraints.FallbackConstraintToggler;
import org.ffplanner.bean.constraints.PriorityChanger;
import org.ffplanner.bean.constraints.SpecificConstraintToggler;
import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserScheduleConstraintsBean extends ConnectorEntityBean<UserScheduleConstraint> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Override
    public UserScheduleConstraint find(Long showingId, Long userScheduleId) {
        return super.find(showingId, userScheduleId);
    }

    @Override
    protected Class<UserScheduleConstraint> getEntityClass() {
        return UserScheduleConstraint.class;
    }

    @Override
    protected Predicate getLeftCondition(Long id, CriteriaBuilder criteriaBuilder, Root<UserScheduleConstraint> root) {
        return criteriaBuilder.equal(root.get(UserScheduleConstraint_.showing).get(Showing_.id), id);
    }

    @Override
    protected Predicate getRightCondition(
            Long id, CriteriaBuilder criteriaBuilder, Root<UserScheduleConstraint> root) {
        return criteriaBuilder.equal(root.get(UserScheduleConstraint_.userSchedule).get(UserSchedule_.id), id);
    }

    /**
     * If {@code constraintType} is set, it is removed. If no constraint is set or any other constraint is set,
     * {@code constraintType} becomes the new constraint.
     */
    public void toggleConstraint(Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType) {
        new SpecificConstraintToggler(entityManager, showing, userSchedule).change(constraintType);
    }

    /**
     * If any constraint is set, it is removed. If no constraint is set, {@code constraintType} becomes the new
     * constraint.
     */
    public void toggleAnyConstraint(Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType) {
        new AnyConstraintToggler(entityManager, showing, userSchedule).change(constraintType);
    }

    /**
     * If {@code constraintType} is set, it replaced with {@code baseConstraintType}. If no constraint is set or any
     * other constraint is set, {@code constraintType} becomes the new constraint.
     */
    public void toggleFallbackConstraint(Showing showing, UserSchedule userSchedule,
            ScheduleConstraintType constraintType, ScheduleConstraintType baseConstraintType) {
        new FallbackConstraintToggler(entityManager, showing, userSchedule, baseConstraintType).change(constraintType);
    }

    public void setConstraintPriority(Showing showing, UserSchedule userSchedule, Short priority) {
        new PriorityChanger(entityManager, showing, userSchedule).change(priority);
    }

    public boolean hasConstraint(Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType) {
        final UserScheduleConstraint constraints = entityManager.find(
                UserScheduleConstraint.class, new UserScheduleConstraintId(userSchedule.getId(), showing.getId()));
        return constraints != null && constraints.getConstraintType() == constraintType;
    }
}
