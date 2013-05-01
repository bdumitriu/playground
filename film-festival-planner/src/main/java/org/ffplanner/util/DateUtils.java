package org.ffplanner.util;

import org.joda.time.DateTime;
import org.joda.time.Interval;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;

/**
 * @author Bogdan Dumitriu
 */
public class DateUtils {

    private static final SimpleDateFormat DURATION_FORMAT = new SimpleDateFormat("HH'h':mm'm'");

    private static final SimpleDateFormat DURATION_FORMAT_MINUTES_ONLY = new SimpleDateFormat("mm'm'");

    public static Date parseHoursAndMinutes(String hoursAndMinutes) throws ParseException {
        try {
            return DURATION_FORMAT.parse(hoursAndMinutes);
        } catch (ParseException e) {
            try {
                return DURATION_FORMAT_MINUTES_ONLY.parse(hoursAndMinutes);
            } catch (ParseException ignored) {
                throw e;
            }
        }
    }

    public static String formatHoursAndMinutes(Date time) {
        return DURATION_FORMAT.format(time);
    }

    public static String formatHoursAndMinutes(int minutes) {
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.set(Calendar.HOUR_OF_DAY, minutes / 60);
        calendar.set(Calendar.MINUTE, minutes % 60);
        return formatHoursAndMinutes(calendar.getTime());
    }

    public static int getInMinutes(Date time) {
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(time);
        return calendar.get(Calendar.HOUR_OF_DAY) * 60 + calendar.get(Calendar.MINUTE);
    }

    public static String getDayOfWeek(Date date, Locale locale) {
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(date);
        return calendar.getDisplayName(Calendar.DAY_OF_WEEK, Calendar.SHORT, locale);
    }

    public static String getDayMonth(Date date, Locale locale) {
        final SimpleDateFormat simpleDateFormat = new SimpleDateFormat("d MMM", locale);
        return simpleDateFormat.format(date);
    }

    public static Interval getDayInterval(Date date) {
        final DateTime dateTime = new DateTime(date);
        return dateTime.toDateMidnight().toInterval();
    }
}
