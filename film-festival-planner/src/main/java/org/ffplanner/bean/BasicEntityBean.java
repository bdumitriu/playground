/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import javax.persistence.metamodel.SingularAttribute;

/**
 * @author Bogdan Dumitriu
 */
public abstract class BasicEntityBean<T> extends EntityBean<T> {

    public T find(Long id) {
        return entityManager.find(getEntityClass(), id);
        /*final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<T> query = criteriaBuilder.createQuery(getEntityClass());
        final Root<T> root = query.from(getEntityClass());
        query.where(criteriaBuilder.equal(root.get(getIdAttribute()), id));
        final TypedQuery<T> typedQuery = entityManager.createQuery(query);
        try {
            return typedQuery.getSingleResult();
        } catch (NoResultException ignored) {
            return null;
        }*/
    }

    public T getReference(Long id) {
        return entityManager.getReference(getEntityClass(), id);
    }

    protected abstract SingularAttribute<T, Long> getIdAttribute();
}