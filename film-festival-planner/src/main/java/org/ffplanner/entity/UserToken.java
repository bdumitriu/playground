/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class UserToken implements Serializable {

    private static final long serialVersionUID = -8495089061944184132L;

    @Id
    private String token;

    @ManyToOne
    private User user;

    public String getToken() {
        return token;
    }

    public void setToken(String token) {
        this.token = token;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }
}
