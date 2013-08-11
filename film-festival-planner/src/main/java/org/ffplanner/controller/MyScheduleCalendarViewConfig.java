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
public class MyScheduleCalendarViewConfig implements CalendarViewConfig {

    @Inject
    @Messages
    private ResourceBundle bundle;

    private final CalendarConfig calendarConfig = new MyScheduleCalendarConfig();

    @Override
    public String getPageName() {
        return "MySchedule";
    }

    @Override
    public String getPageTitle() {
        return bundle.getString("TitleMySchedule");
    }

    @Override
    public CalendarConfig getCalendarConfig() {
        return calendarConfig;
    }
}
