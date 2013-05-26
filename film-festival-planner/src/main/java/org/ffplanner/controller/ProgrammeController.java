package org.ffplanner.controller;

import org.ffplanner.bean.programme.FestivalEditionProgramme;
import org.ffplanner.bean.programme.FestivalProgrammeBean;
import org.ffplanner.entity.FestivalEditionSection;
import org.ffplanner.entity.Movie;
import org.ffplanner.entity.MovieBundleInFestival;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

import static org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_FESTIVAL_EDITION_ID;

/**
 * @author Bogdan Dumitriu
 */
@Named
@RequestScoped
public class ProgrammeController implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private FestivalProgrammeBean festivalProgrammeBean;

    private FestivalEditionSection[] sections;

    private Date sectionsLastInit;

    private FestivalEditionSection section;

    private MovieBundleInFestival movieBundle;

    private Movie movie;

    public FestivalEditionSection[] getSections() {
        if (sections == null || festivalProgrammeBean.changedAfter(sectionsLastInit)) {
            final FestivalEditionProgramme programme =
                    festivalProgrammeBean.getProgrammeFor(DEFAULT_FESTIVAL_EDITION_ID);
            final List<FestivalEditionSection> festivalSections = programme.getSections();
            sections = festivalSections.toArray(new FestivalEditionSection[festivalSections.size()]);
            sectionsLastInit = new Date();
        }
        return sections;
    }

    public void setSection(FestivalEditionSection festivalEditionSection) {
        for (FestivalEditionSection section : sections) {
            if (section.equals(festivalEditionSection)) {
                this.section = section;
                return;
            }
        }
    }

    public FestivalEditionSection getSection() {
        if (section == null) {
            final FestivalEditionSection[] sections = getSections();
            if (sections.length > 0) {
                section = sections[0];
            }
        }
        return section;
    }

    public void setMovieBundle(MovieBundleInFestival movieBundleInFestival) {
        final FestivalEditionSection section = getSection();
        if (section != null) {
            for (MovieBundleInFestival movieBundle : section.getMovieBundles()) {
                if (movieBundle.equals(movieBundleInFestival)) {
                    this.movieBundle = movieBundle;
                    return;
                }
            }
        }
    }

    public MovieBundleInFestival getMovieBundle() {
        if (movieBundle == null) {
            final FestivalEditionSection section = getSection();
            if (section != null) {
                final List<MovieBundleInFestival> movieBundles = section.getMovieBundles();
                if (!movieBundles.isEmpty()) {
                    movieBundle = movieBundles.get(0);
                }
            }
        }
        return movieBundle;
    }
}
