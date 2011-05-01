/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the interface of the NullPointerException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _NULL_POINTER_EXCEPTION_H
#define _NULL_POINTER_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever somebody tries
 * to use a variable which points to NULL.
 *
 * @short Exception thrown whenever an access to variable pointing to NULL is tried.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class NullPointerException : public FSException
{

public:
	/**
	 * Creates a new NullPointerException with a default error message.
	 *
	 * @param methodName the name of the method (recommended: "Class::method")
	 *	where usage of a NULL pointer was tried.
	 */
	NullPointerException(const char* methodName);

	/**
	 * Creates a new NullPointerException with a custom error message.
	 *
	 * @param message the custom error message.
	 * @param methodName the name of the method (recommended: "Class::method")
	 *	where usage of a NULL pointer was tried.
	 */
	NullPointerException(const char* message, const char* methodName);

	/**
	 * The destructor of the class.
	 */
	~NullPointerException();

	/**
	 * Returns the name of the method where usage of a NULL pointer was tried.
	 *
	 * @return the name of the method where usage of a NULL pointer was tried.
	 */
	char* getMethodName();

private:
	char* methodName;
};

#endif
