/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Tue Nov 5 2002
 * Description:		This is the interface of the InvalidDateException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _INVALID_DATE_EXCEPTION_H
#define _INVALID_DATE_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever somebody tries
 * to set a calendaristic date to an invalid value.
 *
 * @short Exception thrown whenever the setting of an incorrect date is tried.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class InvalidDateException : public FSException
{

public:
	/**
	 * Creates a new InvalidDateException with a default error message.
	 *
	 * @param invalidDate the invalid date.
	 */
	InvalidDateException(const char* invalidDate);

	/**
	 * Creates a new InvalidDateException with a custom error message.
	 *
	 * @param message the custom error message.
	 * @param invalidDate the invalid date.
	 */
	InvalidDateException(const char* message, const char* invalidDate);

	/**
	 * The destructor of the class.
	 */
	~InvalidDateException();

	/**
	 * Returns the invalid date.
	 *
	 * @return the invalid date.
	 */
	char* getInvalidDate();

private:
	char* date;
};

#endif
