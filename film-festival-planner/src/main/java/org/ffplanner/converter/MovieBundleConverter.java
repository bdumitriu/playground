/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.converter;

import org.ffplanner.entity.MovieBundle;

import javax.faces.convert.FacesConverter;

/**
 * @author Bogdan Dumitriu
 */
@FacesConverter(value = "movieBundleConverter")
public class MovieBundleConverter extends BasicEntityConverter<MovieBundle> {

    @Override
    protected String getEntityEjbJndiName() {
        return "java:module/MovieBundleBean";
    }

    @Override
    protected Long getId(Object value) {
        return ((MovieBundle) value).getId();
    }
}
