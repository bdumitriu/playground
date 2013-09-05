package org.ffplanner.controller;

import org.ffplanner.qualifier.Messages;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.ResourceBundle;

/**
 * @author Bogdan Dumitriu
 */
@Named
@ApplicationScoped
public class ProgramCalendarViewConfig implements CalendarViewConfig {

    @Inject
    @Messages
    private ResourceBundle bundle;

    @Inject
    private ProgramCalendarConfig calendarConfig;

    @Override
    public String getPageName() {
        return "Program";
    }

    @Override
    public String getPageTitle() {
        return bundle.getString("TitleProgram");
    }

    @Override
    public CalendarConfig getCalendarConfig() {
        return calendarConfig;
    }
}
