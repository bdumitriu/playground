/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.converter;

import org.ffplanner.entity.Section;

import javax.faces.convert.FacesConverter;

/**
 * @author Bogdan Dumitriu
 */
@FacesConverter("sectionConverter")
public class SectionConverter extends EntityConverter<Section> {

    @Override
    protected String getEntityEjbJndiName() {
        return "java:module/SectionEJB";
    }

    @Override
    protected Long getId(Object value) {
        return ((Section) value).getId();
    }
}
