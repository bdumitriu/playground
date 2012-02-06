/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Section implements Serializable {

    private static final long serialVersionUID = 1L;

    @GeneratedValue
    @Id
    private Long id;

    private String name;

    public Section() {
    }

    public Section(String name) {
        this.name = name;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
