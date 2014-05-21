/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package jboss;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Foo {

    @Id
    @GeneratedValue
    private Long id;

    private String name;

    public Foo(String name) {
        this.name = name;
    }
}
