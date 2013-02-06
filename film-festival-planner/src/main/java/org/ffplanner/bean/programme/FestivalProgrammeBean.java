package org.ffplanner.bean.programme;

import org.ffplanner.bean.FestivalEditionBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.Showing;

import javax.annotation.PostConstruct;
import javax.ejb.*;
import javax.inject.Inject;
import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

    @PostConstruct
    public void initialize() {
        final List<FestivalEdition> festivalEditions = festivalEditionBean.findAll();
        for (FestivalEdition festivalEdition : festivalEditions) {
            festivalEdition.loadLazyFields();
            final List<Showing> festivalShowings = loadFestivalShowings(festivalEdition);
            programmes.put(festivalEdition, new FestivalEditionProgramme(festivalEdition, festivalShowings));
        }
    }

    private List<Showing> loadFestivalShowings(FestivalEdition festivalEdition) {
        final List<Showing> festivalShowings = showingBean.findBy(festivalEdition);
        for (Showing showing : festivalShowings) {
            showing.loadLazyFields();
        }
        return festivalShowings;
    }

    public FestivalEditionProgramme getProgrammeFor(Long festivalEditionId) {
        return getProgrammeFor(festivalEditionBean.find(festivalEditionId));
    }

    public FestivalEditionProgramme getProgrammeFor(FestivalEdition festivalEdition) {
        return programmes.get(festivalEdition);
    }
}
