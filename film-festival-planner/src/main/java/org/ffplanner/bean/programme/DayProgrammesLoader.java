package org.ffplanner.bean.programme;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import org.ffplanner.entity.Showing;
import org.joda.time.Interval;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class DayProgrammesLoader {

    private final List<Showing> showings;

    private final Multimap<Interval, Showing> dayShowings;

    private final Map<Interval, DayProgramme> dayProgrammes;

    public DayProgrammesLoader(List<Showing> festivalShowings) {
        this.showings = festivalShowings;
        this.dayShowings = ArrayListMultimap.create();
        this.dayProgrammes = new HashMap<>();
        loadShowings();
    }

    public Map<Interval, DayProgramme> getDayProgrammes() {
        return dayProgrammes;
    }

    private void loadShowings() {
        splitShowingsByDay();
        splitDayShowingsByVenueAndHour();
    }

    private void splitShowingsByDay() {
        for (Showing showing : showings) {
            dayShowings.put(showing.getDayInterval(), showing);
        }
    }

    private void splitDayShowingsByVenueAndHour() {
        for (Map.Entry<Interval, Collection<Showing>> entry : dayShowings.asMap().entrySet()) {
            // dayShowings is an ArrayListMultimap (keys behave like HashMap, values like ArrayList). The festival
            // edition showings are obtained in venue- (and time-)sorted order. Hence, entry.getValue() below returns a
            // day's showings in venue-sorted order
            dayProgrammes.put(entry.getKey(), new DayProgramme(entry.getValue()));
        }
    }
}
