package org.ffplanner.converter;

import org.ffplanner.entity.FestivalEditionSection;
import org.ffplanner.entity.MovieBundleInFestival;

/**
 * @author Bogdan Dumitriu
 */
public final class Converters {

    public static String convertSection(FestivalEditionSection section) {
        return new FestivalEditionSectionConverter().getAsString(section);
    }

    public static String convertMovieBundleInFestival(MovieBundleInFestival movieBundleInFestival) {
        return new MovieBundleInFestivalConverter().getAsString(movieBundleInFestival);
    }
}
