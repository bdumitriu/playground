/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Tue Nov 5 2002
 * Description:		This is the interface of the Date class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _DATE_H
#define _DATE_H

#include <time.h>
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include "Utils.h"
#include "../exceptions/InvalidDateException.h"

/**
 * This class represents a simple calendaristic date (day, month, year).
 *
 * @short Represents a simple calendaristic date (day, month, year).
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class Date
{

public:
	/**
	 * Creates a new Date and initializes it with the current
	 * system date.
	 */
	Date();

	/**
	 * A copy constructor.
	 */
	Date(const Date& date);

	/**
	 * Creates a new Date and initializes it using the received
	 * parameters. If the date is invalid, the date is set to
	 * the current system date and an InvalidDateException is
	 * thrown.
	 *
	 * The method throws:
	 * <ul>
	 * <li> InvalidDateException* if the date is invalid.</li>
	 * </ul>
	 *
	 * @param day the day of the date.
	 * @param month the month of the date.
	 * @param year the year of the date.
	 */
	Date(unsigned char day, unsigned char month, unsigned short year)
		throw(InvalidDateException*);

	/**
	 * Returns the value of the day data member.
	 *
	 * @return the value of the day data member.
	 */
	unsigned char getDay();

	/**
	 * Returns the value of the month data member.
	 *
	 * @return the value of the month data member.
	 */
	unsigned char getMonth();

	/**
	 * Returns the value of the year data member.
	 *
	 * @return the value of the year data member.
	 */
	unsigned short getYear();

	/**
	 * Sets the day data member to <code>day</code>. If the
	 * date would become invalid by setting the data member
	 * to the requested value, an InvalidDateException is
	 * thrown and the data member's value is left unchanged.
	 *
	 * The method throws:
	 * <ul>
	 * <li> InvalidDateException* if the date would become invalid
	 *	by setting the data member to the requested value.</li>
	 * </ul>
	 *
	 * @param day the new value of the day data member.
	 */
	void setDay(unsigned char day) throw(InvalidDateException*);

	/**
	 * Sets the month data member to <code>month</code>. If the
	 * date would become invalid by setting the data member
	 * to the requested value, an InvalidDateException is
	 * thrown and the data member's value is left unchanged.
	 *
	 * The method throws:
	 * <ul>
	 * <li> InvalidDateException* if the date would become invalid
	 *	by setting the data member to the requested value.</li>
	 * </ul>
	 *
	 * @param month the new value of the month data member.
	 */
	void setMonth(unsigned char month) throw(InvalidDateException*);

	/**
	 * Sets the year data member to <code>year</code>. If the
	 * date would become invalid by setting the data member
	 * to the requested value, an InvalidDateException is
	 * thrown and the data member's value is left unchanged.
	 *
	 * The method throws:
	 * <ul>
	 * <li> InvalidDateException* if the date would become invalid
	 *	by setting the data member to the requested value.</li>
	 * </ul>
	 *
	 * @param year the new value of the year data member.
	 */
	void setYear(unsigned short year) throw(InvalidDateException*);

	/**
	 * Returns the date as a 4 byte array, with the first byte
	 * representing the day, the second byte representing the
	 * month and the last two bytes representing the year.
	 *
	 * @return the date as a 4 byte array.
	 */
	unsigned char* getAsBytes();

	/**
	 * Sets all the date's data members using the 4 byte representation
	 * received as parameter. The 4 byte array is interpreted as follows:
	 * the first byte is thought to represent the day, the second byte
	 * is thought to represent the month and the last two bytes are
	 * thought to represent the year. If an invalid date results from
	 * the conversion, an InvalidDateException is thrown and all of the
	 * data members are left unchanged.
	 *
	 * The method throws:
	 * <ul>
	 * <li> InvalidDateException* if the converted date is invalid.</li>
	 * </ul>
	 *
	 * @param bytes a 4 byte array containing the date in the format
	 *	described above.
	 */
	void setFromBytes(unsigned char* bytes) throw(InvalidDateException*);

	/**
	 * Returns a string representation of this date as D/M/Y.
	 *
	 * @return a string representation of this date as D/M/Y.
	 */
	char* toString();

private:
	unsigned short year;
	unsigned char month;
	unsigned char day;
};

#endif
