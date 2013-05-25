package org.ffplanner.bean;

import org.ffplanner.entity.Section;
import org.ffplanner.entity.Section_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class SectionBean extends BasicEntityBean<Section> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Override
    protected Class<Section> getEntityClass() {
        return Section.class;
    }

    @Override
    protected SingularAttribute<Section, Long> getIdAttribute() {
        return Section_.id;
    }

    public Section findOrCreateBy(String sectionName) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Section> query = criteriaBuilder.createQuery(Section.class);
        final Root<Section> root = query.from(Section.class);
        query.where(criteriaBuilder.equal(root.get(Section_.name), sectionName));
        final TypedQuery<Section> sectionQuery = entityManager.createQuery(query);
        final List<Section> result = sectionQuery.getResultList();
        final Section section;
        if (result.isEmpty()) {
            section = new Section(sectionName);
            entityManager.persist(section);
        } else {
            assert result.size() == 1;
            section = result.get(0);
        }
        return section;
    }

    public List<Section> getSections() {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Section> query = criteriaBuilder.createQuery(Section.class);
        query.from(Section.class);
        final TypedQuery<Section> sectionQuery = entityManager.createQuery(query);
        return sectionQuery.getResultList();
    }
}
