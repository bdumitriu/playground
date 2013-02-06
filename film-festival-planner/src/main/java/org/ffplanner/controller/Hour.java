package org.ffplanner.controller;

import com.google.common.collect.ObjectArrays;

import static com.google.common.base.Preconditions.checkArgument;
import static java.util.Arrays.copyOfRange;
import static org.joda.time.DateTimeConstants.HOURS_PER_DAY;

/**
 * @author Bogdan Dumitriu
 */
public enum Hour {

    HOUR_0,
    HOUR_1,
    HOUR_2,
    HOUR_3,
    HOUR_4,
    HOUR_5,
    HOUR_6,
    HOUR_7,
    HOUR_8,
    HOUR_9,
    HOUR_10,
    HOUR_11,
    HOUR_12,
    HOUR_13,
    HOUR_14,
    HOUR_15,
    HOUR_16,
    HOUR_17,
    HOUR_18,
    HOUR_19,
    HOUR_20,
    HOUR_21,
    HOUR_22,
    HOUR_23;

    private static final Hour[] ALL_HOURS = Hour.values();

    public static Hour getHour(int hour) {
        checkArgument(hour >= 0 && hour < HOURS_PER_DAY, "The hour was expected to be in the interval [0, 24)");
        return ALL_HOURS[hour];
    }

    /**
     * @return an array with all the hours in a day.
     */
    public static Hour[] allHoursInDay() {
        return ALL_HOURS;
    }

    /**
     * The hours between {@code from} and {@code to}. If {@code from} > {@code to}, the hours will wrap over.
     * For example:
     * <pre>
     *     Hour[] hours = Hour.from(19, 2); // hours = {19, 20, 21, 22, 23, 0, 1}
     * </pre>
     *
     * @param from
     *            first hour to include (inclusive)
     * @param to
     *            last hour to include (exclusive)
     * @return an array with all the hours in the interval {@code [from, to}).
     */
    public static Hour[] hoursFrom(int from, int to) {
        if (from <= 0) {
            from = 0;
        }
        if (to <= 0) {
            to = 0;
        }
        if (from >= HOURS_PER_DAY) {
            from = HOURS_PER_DAY;
        }
        if (to >= HOURS_PER_DAY) {
            to = HOURS_PER_DAY;
        }
        if (from < to) {
            return copyOfRange(ALL_HOURS, from, to);
        } else {
            return ObjectArrays.concat(
                    copyOfRange(ALL_HOURS, from, HOURS_PER_DAY), copyOfRange(ALL_HOURS, 0, to), Hour.class);
        }
    }

    public int getHour() {
        return ordinal();
    }

    public String getFormatted() {
        return String.format("%d:00", getHour());
    }
}
