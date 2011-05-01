/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 2 2002
 * Description:		This is the interface of the FSException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _FS_EXCEPTION_H
#define _FS_EXCEPTION_H

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <strstream.h>

/**
 * This class represents an exception that is raised whenever a
 * hard disk access fails.
 *
 * @short Exception thrown whenever a hard disk access fails.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class FSException
{

public:
	/**
	 * Creates a new FSException with a default error message.
	 */
	FSException();

	/**
	 * Creates a new FSException with a custom error message.
	 *
	 * @param message the custom error message.
	 */
	FSException(const char* message);

	/**
	 * The destructor of the class.
	 */
	virtual ~FSException();

	/**
	 * Returns a string containing a description of the exception.
	 *
	 * @return a string containing a description of the exception.
	 */
	virtual char* getMessage();

protected:
	/**
	 * Sets the message data member to <code>message</code>.
	 *
	 * @param message the new value of the message data member.
	 */
	void setMessage(const char* message);

private:
	char* message;

};

#endif
