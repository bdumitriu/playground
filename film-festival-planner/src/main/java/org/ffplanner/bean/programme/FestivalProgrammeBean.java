package org.ffplanner.bean.programme;

import org.ffplanner.ScheduleBuilder;
import org.ffplanner.bean.FestivalEditionBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.def.FestivalProgrammeDefinition;
import org.ffplanner.def.ShowingDefinition;
import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.Showing;

import javax.annotation.PostConstruct;
import javax.ejb.*;
import javax.inject.Inject;
import java.io.Serializable;
import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
@Startup
@Singleton
@LocalBean
@Lock(LockType.READ)
public class FestivalProgrammeBean implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private FestivalEditionBean festivalEditionBean;

    @Inject
    private ShowingBean showingBean;

    private final Map<FestivalEdition, FestivalEditionProgramme> programmes = new HashMap<>();

    private final Map<FestivalEdition, ScheduleBuilder> scheduleBuilders = new HashMap<>();

    private Date lastInit;

    @PostConstruct
    public synchronized void reinitialize() {
        programmes.clear();
        scheduleBuilders.clear();
        final List<FestivalEdition> festivalEditions = festivalEditionBean.findAll();
        for (FestivalEdition festivalEdition : festivalEditions) {
            festivalEdition.loadLazyFields();
            final List<Showing> festivalShowings = loadFestivalShowings(festivalEdition);
            programmes.put(festivalEdition, new FestivalEditionProgramme(festivalEdition, festivalShowings));
            scheduleBuilders.put(
                    festivalEdition, new ScheduleBuilder(getFestivalProgrammeDefinition(festivalShowings)));
        }
        lastInit = new Date();
    }

    public synchronized boolean changedAfter(Date date) {
        assert lastInit != null;
        return date == null || lastInit == null || lastInit.after(date);
    }

    public synchronized FestivalEditionProgramme getProgrammeFor(Long festivalEditionId) {
        return getProgrammeFor(festivalEditionBean.find(festivalEditionId));
    }

    public synchronized FestivalEditionProgramme getProgrammeFor(FestivalEdition festivalEdition) {
        return programmes.get(festivalEdition);
    }

    public synchronized ScheduleBuilder getScheduleBuilder(Long festivalEditionId) {
        return getScheduleBuilder(festivalEditionBean.find(festivalEditionId));
    }

    private ScheduleBuilder getScheduleBuilder(FestivalEdition festivalEdition) {
        return scheduleBuilders.get(festivalEdition);
    }

    private List<Showing> loadFestivalShowings(FestivalEdition festivalEdition) {
        final List<Showing> festivalShowings = showingBean.findBy(festivalEdition);
        for (Showing showing : festivalShowings) {
            showing.loadLazyFields();
        }
        return festivalShowings;
    }

    private FestivalProgrammeDefinition getFestivalProgrammeDefinition(final List<Showing> festivalShowings) {
        return new FestivalProgrammeDefinition() {

            private List<ShowingDefinition> showingDefinitions;

            @Override
            public List<ShowingDefinition> getShowings() {
                if (showingDefinitions == null) {
                    showingDefinitions = new LinkedList<>();
                    for (Showing festivalShowing : festivalShowings) {
                        showingDefinitions.add(festivalShowing);
                    }
                }
                return showingDefinitions;
            }
        };
    }
}
