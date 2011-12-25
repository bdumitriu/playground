/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Person {

    @Id
    @GeneratedValue
    private Long id;

    private String name;

    public Person() {
    }

    public Person(String name) {
        this.name = name;
    }
}
