/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package jboss;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class FooEJB {

    @PersistenceContext(unitName = "foo")
    private EntityManager entityManager;

    public void addFoo() {
        entityManager.persist(new Foo("bar"));
    }
}
