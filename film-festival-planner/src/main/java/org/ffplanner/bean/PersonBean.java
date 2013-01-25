/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.Person;
import org.ffplanner.entity.Person_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class PersonBean {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    public Person findOrCreateActorBy(String actorName) {
        return findOrCreateBy(actorName);
    }

    public Person findOrCreateDirectorBy(String directorName) {
        return findOrCreateBy(directorName);
    }

    private Person findOrCreateBy(String personName) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Person> query = criteriaBuilder.createQuery(Person.class);
        final Root<Person> root = query.from(Person.class);
        query.where(criteriaBuilder.equal(root.get(Person_.name), personName));
        final TypedQuery<Person> countryQuery = entityManager.createQuery(query);
        final List<Person> result = countryQuery.getResultList();
        final Person person;
        if (result.isEmpty()) {
            person = new Person(personName);
        } else {
            assert result.size() == 1;
            person = result.get(0);
        }
        return person;
    }
}
