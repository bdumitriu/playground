package com.one.presentation;

import com.one.business.sessions.boundary.SessionProvider;

import javax.enterprise.inject.Model;
import javax.inject.Inject;

/**
 * @author Bogdan Dumitriu
 */
@Model
public class Index {

    @Inject
    SessionProvider sessionProvider;

    public String getSession() {
        return sessionProvider.getSession();
    }
}
