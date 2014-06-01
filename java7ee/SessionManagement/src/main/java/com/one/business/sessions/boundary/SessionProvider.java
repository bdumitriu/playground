package com.one.business.sessions.boundary;

import com.one.business.sessions.control.Archiver;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
public class SessionProvider {

    @Inject
    Archiver archiver;

    @PersistenceContext
    EntityManager entityManager;

    public String getSession() {
        return "java";
    }
}
