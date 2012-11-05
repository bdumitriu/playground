package org.ffplanner.controller;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public class Hour implements Serializable, Comparable<Hour> {

    private final int hour;

    public Hour(int hour) {
        this.hour = hour;
    }

    public int getHour() {
        return hour;
    }

    public String getFormatted() {
        return String.format("%d:00", hour);
    }

    @Override
    public int compareTo(Hour other) {
        return Integer.compare(hour, other.hour);
    }
}
