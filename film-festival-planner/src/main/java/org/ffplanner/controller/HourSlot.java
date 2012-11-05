package org.ffplanner.controller;

import org.ffplanner.entity.Showing;

/**
 * @author Bogdan Dumitriu
 */
public class HourSlot {

    private Hour hour;

    private Showing showing;

    public HourSlot(Hour hour, Showing showing) {
        this.hour = hour;
        this.showing = showing;
    }

    public Hour getHour() {
        return hour;
    }

    public void setHour(Hour hour) {
        this.hour = hour;
    }

    public Showing getShowing() {
        return showing;
    }

    public void setShowing(Showing showing) {
        this.showing = showing;
    }

    public boolean hasShowing() {
        return this.showing != null;
    }

    /**
     * @param hourCellHeight
     *            the height (in pixels) of the cell representing the hour
     * @return the number of margin pixels to use in order to correctly position a div representing the showing of this
     *         slot in a cell representing the hour.
     */
    public int getMargin(int hourCellHeight) {
        if (showing == null) {
            return 0;
        } else {
            return showing.getMinute() * hourCellHeight / 60;
        }
    }

    public int getHeight(int hourCellHeight) {
        if (showing == null) {
            return 0;
        } else {
            final int durationInMinutes = showing.getMovieBundle().getDurationInMinutes();
            return durationInMinutes * hourCellHeight / 60;
        }
    }
}
