/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

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

    @OneToMany(mappedBy = "section")
    @OrderBy
    private List<MovieBundle> movieBundles;

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

    public List<MovieBundle> getMovieBundles() {
        return movieBundles;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Section section = (Section) o;

        if (id != null ? !id.equals(section.id) : section.id != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }
}
