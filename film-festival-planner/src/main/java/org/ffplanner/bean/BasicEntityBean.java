package org.ffplanner.bean;

import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
public abstract class BasicEntityBean<T> extends EntityBean<T> {

    public T find(Long id) {
        return entityManager.find(getEntityClass(), id);
    }

    public T getReference(Long id) {
        return entityManager.getReference(getEntityClass(), id);
    }

    public List<T> findAll() {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<T> query = criteriaBuilder.createQuery(getEntityClass());
        final Root<T> root = query.from(getEntityClass());
        final TypedQuery<T> typedQuery = entityManager.createQuery(query);
        return typedQuery.getResultList();
    }

    protected abstract SingularAttribute<T, Long> getIdAttribute();
}
