package org.ffplanner.bean.programme;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import org.ffplanner.entity.FestivalEditionVenue;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;
import org.joda.time.Interval;

import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
public class DayProgrammesLoader {

    private final List<Showing> showings;

    private final VenueComparator venueComparator;

    private final Multimap<Interval, Showing> dayShowings;

    private final Map<Interval, DayProgramme> dayProgrammes;

    public DayProgrammesLoader(List<Showing> festivalShowings, List<FestivalEditionVenue> festivalVenues) {
        this.showings = festivalShowings;
        this.venueComparator = new VenueComparator(festivalVenues);
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
            dayProgrammes.put(entry.getKey(), new DayProgramme(entry.getValue(), venueComparator));
        }
    }

    private static class VenueComparator implements Comparator<Venue> {

        private final Map<Venue, Integer> festivalVenues = new HashMap<>();

        private VenueComparator(Iterable<FestivalEditionVenue> festivalVenues) {
            int index = 0;
            for (FestivalEditionVenue festivalVenue : festivalVenues) {
                this.festivalVenues.put(festivalVenue.getVenue(), index++);
            }
        }

        @Override
        public int compare(Venue venue1, Venue venue2) {
            final Integer venue1Index = festivalVenues.get(venue1);
            final Integer venue2Index = festivalVenues.get(venue2);
            if (venue1Index != null && venue2Index != null) {
                return Integer.compare(venue1Index, venue2Index);
            } else if (venue1Index != null) {
                return -1;
            } else if (venue2Index != null) {
                return 1;
            } else {
                return 0;
            }
        }
    }
}
