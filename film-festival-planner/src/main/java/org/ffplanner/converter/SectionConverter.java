package org.ffplanner.converter;

import org.ffplanner.entity.Section;

import javax.faces.convert.FacesConverter;

/**
 * @author Bogdan Dumitriu
 */
@FacesConverter("sectionConverter")
public class SectionConverter extends BasicEntityConverter<Section> {

    @Override
    protected Class<Section> getEntityClass() {
        return Section.class;
    }

    @Override
    protected String getEntityEjbJndiName() {
        return "java:module/SectionBean";
    }

    @Override
    protected Long getId(Section value) {
        return value.getId();
    }
}
