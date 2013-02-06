package org.ffplanner.bean;

import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
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

    public FestivalEditionSection find(Long festivalEditionId, Section section) {
        return find(festivalEditionId, section.getId());
    }
}
