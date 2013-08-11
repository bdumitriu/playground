package org.ffplanner.controller;

import org.ffplanner.qualifier.Messages;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.ResourceBundle;

/**
 * @author Bogdan Dumitriu
 */
@Named
@RequestScoped
public class ProgramCalendarViewConfig implements CalendarViewConfig {

    @Inject
    @Messages
    private ResourceBundle bundle;

    private final CalendarConfig calendarConfig = new ProgramCalendarConfig();

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
