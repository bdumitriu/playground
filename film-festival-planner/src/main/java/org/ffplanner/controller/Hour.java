package org.ffplanner.controller;

/**
 *
 *
 * @author Bogdan Dumitriu
 */
public class Hour {

	private int hour;

	public Hour(int hour) {
		this.hour = hour;
	}

	public int getHour() {
		return hour;
	}

	public String getFormatted() {
		return String.format("%d:00", hour);
	}
}
