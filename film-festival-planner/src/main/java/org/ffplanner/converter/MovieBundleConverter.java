package org.ffplanner.converter;

import org.ffplanner.entity.MovieBundle;

import javax.faces.convert.FacesConverter;

/**
 * @author Bogdan Dumitriu
 */
@FacesConverter(value = "movieBundleConverter")
public class MovieBundleConverter extends BasicEntityConverter<MovieBundle> {

    @Override
    protected Class<MovieBundle> getEntityClass() {
        return MovieBundle.class;
    }

    @Override
    protected String getEntityEjbJndiName() {
        return "java:module/MovieBundleBean";
    }

    @Override
    protected Long getId(MovieBundle value) {
        return value.getId();
    }
}
