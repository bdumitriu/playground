/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class Country {

	@Id
	@GeneratedValue
	private Long id;

	@Column(unique = true)
	private String name;

	public Country() {
	}

	public Country(String name) {
		this.name = name;
	}
}
