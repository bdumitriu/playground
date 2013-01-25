/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * @author Bogdan Dumitriu
 */
public abstract class EntityBean<T> {

    @PersistenceContext(unitName = "ffp")
    protected EntityManager entityManager;

    protected abstract Class<T> getEntityClass();
}
