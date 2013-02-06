package org.ffplanner.converter;

import org.ffplanner.entity.FestivalEditionSection;

import javax.faces.convert.FacesConverter;

/**
 * @author Bogdan Dumitriu
 */
@FacesConverter("festivalEditionSectionConverter")
public class FestivalEditionSectionConverter extends ConnectorEntityConverter<FestivalEditionSection> {

    @Override
    protected Class<FestivalEditionSection> getEntityClass() {
        return FestivalEditionSection.class;
    }

    @Override
    protected String getEntityEjbJndiName() {
        return "java:module/FestivalEditionSectionBean";
    }

    @Override
    protected Long getLeftId(FestivalEditionSection value) {
        return value.getFestivalEdition().getId();
    }

    @Override
    protected Long getRightId(FestivalEditionSection value) {
        return value.getSection().getId();
    }
}
