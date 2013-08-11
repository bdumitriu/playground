package org.ffplanner.controller;

/**
 * @author Bogdan Dumitriu
 */
public abstract class DefaultCalendarConfig implements CalendarConfig {

    @Override
    public String getCalendarFormId() {
        return "sch_form";
    }

    @Override
    public String getRenderOnMovieChange() {
        return ":movieInfo";
    }

    @Override
    public String getOnEventOnMovieChange() {
        return "updateMovieInfo";
    }

    @Override
    public String getRenderOnConstraintsChange() {
        return ":buttons_form";
    }

    @Override
    public String getOnEventOnConstraintsChange() {
        return "repairCalendarAfterAjaxCall";
    }
}
