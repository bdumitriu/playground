/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Fri Nov 8 2002
 * Description:		This is the interface of the NotEnoughSpaceException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _NOT_ENOUGH_SPACE_EXCEPTION_H
#define _NOT_ENOUGH_SPACE_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever the hard disk
 * does not have enough space to accomodate new data.
 *
 * @short Exception thrown whenever hard disk runs out of space.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class NotEnoughSpaceException : public FSException
{

public:

	/**
	 * Creates a new NotEnoughSpaceException with a default error message.
	 */
	NotEnoughSpaceException();

	/**
	 * Creates a new NotEnoughSpaceException with a custom error message.
	 *
	 * @param message the custom error message.
	 */
	NotEnoughSpaceException(const char* message);
};

#endif
