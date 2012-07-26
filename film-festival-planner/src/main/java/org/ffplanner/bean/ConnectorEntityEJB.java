/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import javax.persistence.NoResultException;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

/**
 * @author Bogdan Dumitriu
 */
public abstract class ConnectorEntityEJB<T> extends EntityEJB<T> {

    public T find(Long idLeft, Long idRight) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<T> query = criteriaBuilder.createQuery(getEntityClass());
        final Root<T> root = query.from(getEntityClass());
        query.where(criteriaBuilder.and(
                getLeftCondition(idLeft, criteriaBuilder, root), getRightCondition(idRight, criteriaBuilder, root)));
        final TypedQuery<T> typedQuery = entityManager.createQuery(query);
        try {
            return typedQuery.getSingleResult();
        } catch (NoResultException ignored) {
            return null;
        }
    }

    protected abstract Predicate getLeftCondition(Long id, CriteriaBuilder criteriaBuilder, Root<T> root);

    protected abstract Predicate getRightCondition(Long id, CriteriaBuilder criteriaBuilder, Root<T> root);
}
