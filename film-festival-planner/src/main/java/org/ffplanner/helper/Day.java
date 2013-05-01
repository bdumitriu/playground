package org.ffplanner.helper;

import org.ffplanner.converter.DayConverter;
import org.ffplanner.util.DateUtils;
import org.joda.time.DateTime;

import java.util.Date;
import java.util.Locale;

/**
 * @author Bogdan Dumitriu
 */
public class Day {

    private final Date date;

    public Day(DateTime dateTime) {
        this.date = dateTime.toDate();
    }

    public Date getDate() {
        return date;
    }

    public String getDayOfWeek(Locale locale) {
        return DateUtils.getDayOfWeek(date, locale);
    }

    public String getDayMonth(Locale locale) {
        return DateUtils.getDayMonth(date, locale);
    }

    public String getAsString() {
        return new DayConverter().getAsString(null, null, date);
    }
}
