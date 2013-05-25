package org.ffplanner.bean;

import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class FestivalEditionSectionBean extends ConnectorEntityBean<FestivalEditionSection> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private FestivalEditionBean festivalEditionBean;

    @Inject
    private SectionBean sectionBean;

    @Override
    protected Class<FestivalEditionSection> getEntityClass() {
        return FestivalEditionSection.class;
    }

    @Override
    protected Predicate getLeftCondition(Long id, CriteriaBuilder criteriaBuilder, Root<FestivalEditionSection> root) {
        return criteriaBuilder.equal(root.get(FestivalEditionSection_.festivalEdition).get(FestivalEdition_.id), id);
    }

    @Override
    protected Predicate getRightCondition(Long id, CriteriaBuilder criteriaBuilder, Root<FestivalEditionSection> root) {
        return criteriaBuilder.equal(root.get(FestivalEditionSection_.section).get(Section_.id), id);
    }

    @Override
    public FestivalEditionSection find(Long festivalEditionId, Long sectionId) {
        return super.find(festivalEditionId, sectionId);
    }

    public FestivalEditionSection findOrCreate(Long festivalEditionId, String sectionName) {
        final Section section = sectionBean.findOrCreateBy(sectionName);
        FestivalEditionSection festivalEditionSection = find(festivalEditionId, section.getId());
        if (festivalEditionSection == null) {
            festivalEditionSection = new FestivalEditionSection();
            festivalEditionSection.setFestivalEdition(festivalEditionBean.find(festivalEditionId));
            festivalEditionSection.setSection(section);
            entityManager.persist(festivalEditionSection);
        }
        return festivalEditionSection;
    }
}
