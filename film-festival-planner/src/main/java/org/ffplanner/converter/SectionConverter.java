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
public class SectionConverter extends BasicEntityConverter<Section> {

    @Override
    protected String getEntityEjbJndiName() {
        return "java:module/SectionBean";
    }

    @Override
    protected Long getId(Object value) {
        return ((Section) value).getId();
    }
}
