/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;

/**
 * @author Bogdan Dumitriu
 */
public abstract class EntityEJB<T> {

    @PersistenceContext(unitName = "ffp")
    protected EntityManager entityManager;

    public T find(Long id) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<T> query = criteriaBuilder.createQuery(getEntityClass());
        final Root<T> root = query.from(getEntityClass());
        query.where(criteriaBuilder.equal(root.get(getIdAttribute()), id));
        final TypedQuery<T> typedQuery = entityManager.createQuery(query);
        try {
            return typedQuery.getSingleResult();
        } catch (NoResultException ignored) {
            return null;
        }
    }

    protected abstract Class<T> getEntityClass();

    protected abstract SingularAttribute<T, Long> getIdAttribute();
}
