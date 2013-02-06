package org.ffplanner.bean.programme;

import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.FestivalEditionSection;
import org.ffplanner.entity.Showing;
import org.ffplanner.util.DateUtils;
import org.joda.time.Interval;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class FestivalEditionProgramme {

    private final FestivalEdition festivalEdition;

    private final Map<Interval, DayProgramme> dayProgrammes;

    private final List<FestivalEditionSection> sectionMovies;

    public FestivalEditionProgramme(FestivalEdition festivalEdition, List<Showing> festivalShowings) {
        this.festivalEdition = festivalEdition;
        this.dayProgrammes = new DayProgrammesLoader(festivalShowings).getDayProgrammes();
        this.sectionMovies = festivalEdition.getSections();
    }

    public DayProgramme getDayProgramme(Date date) {
        return getDayProgramme(DateUtils.getDayInterval(date));
    }

    public DayProgramme getDayProgramme(Interval day) {
        final DayProgramme dayProgramme = dayProgrammes.get(day);
        return dayProgramme == null ? DayProgramme.getEmptyProgramme() : dayProgramme;
    }

    public List<FestivalEditionSection> getSections() {
        return sectionMovies;
    }
}
