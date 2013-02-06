package org.ffplanner.converter;

import org.ffplanner.bean.MovieBundleInFestivalBean;
import org.ffplanner.entity.MovieBundleInFestival;

import javax.faces.convert.FacesConverter;
import javax.naming.InitialContext;
import javax.naming.NamingException;

/**
 * @author Bogdan Dumitriu
 */
@FacesConverter("movieBundleInFestivalConverter")
public class MovieBundleInFestivalConverter extends EntityConverter<MovieBundleInFestival> {

    @Override
    protected Class<MovieBundleInFestival> getEntityClass() {
        return MovieBundleInFestival.class;
    }

    @Override
    protected MovieBundleInFestival getAsObject(String value, InitialContext initialContext) throws NamingException {
        final MovieBundleInFestivalBean movieBundleInFestivalBean =
                (MovieBundleInFestivalBean) initialContext.lookup("java:module/MovieBundleInFestivalBean");
        try {
            final String[] ids = value.split("\\|");
            if (ids.length != 3) {
                return null;
            }
            final Long movieBundleId = Long.valueOf(ids[0]);
            final Long festivalEditionId = Long.valueOf(ids[1]);
            final Long sectionId = Long.valueOf(ids[2]);
            return movieBundleInFestivalBean.find(movieBundleId, festivalEditionId, sectionId);
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    @Override
    public String getAsString(MovieBundleInFestival value) {
        final StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(value.getMovieBundle().getId());
        stringBuilder.append("|");
        stringBuilder.append(value.getFestivalEditionSection().getFestivalEdition().getId());
        stringBuilder.append("|");
        stringBuilder.append(value.getFestivalEditionSection().getSection().getId());
        return stringBuilder.toString();
    }
}
