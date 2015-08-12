package org.ffplanner.controller;

/**
 * @author Bogdan Dumitriu
 */
public interface CalendarConfig {

    /**
     * @return should always return "sch_form". Normally this should be a constant, but constants are not supported in
     *         EL <3.0
     * TODO: after switch to JavaEE 7, replace with constant and refer from EL as #{T(org.ffplanner.controller.CalendarViewConfig).CALENDARD_FORM_ID}
     */
    String getCalendarFormId();

    String getRenderOnMovieChange();

    String getOnEventOnMovieChange();

    String getRenderOnConstraintsChange();

    /**
     * @return should always return "repairCalendarAfterAjaxCall". Normally this should be a constant, but constants are
     *         not supported in EL <3.0
     * TODO: after switch to JavaEE 7, replace with constant and refer from EL as #{T(org.ffplanner.controller.CalendarViewConfig).ON_EVENT_ON_CONSTRAINTS_CHANGE}
     */
    String getOnEventOnConstraintsChange();

    boolean showMovieId(Long showingId);

    String getMovieStyleClass(Long showingId);

    boolean showWatchAnyFor(Long showingId);

    boolean showRemoveInterestFor(Long showingId);

    boolean showPriorityControlFor(Long showingId);

    boolean showWatchThisFor(Long showingId);

    boolean showWatchThisSelectedByUserFor(Long showingId);

    boolean showWatchThisSelectedBySystemFor(Long showingId);
}
