/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Oct 27 2002
 * Description:		This is the interface of the ArrayIndexOutOfBoundsException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 **************************************************************************/

#ifndef _ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION_H
#define _ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever somebody tries
 * to access an element of an array whose index is either less than 0 or
 * greater than or equal to the array's size.
 *
 * @short Exception thrown whenever an access to an invalid array element is tried.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */

class ArrayIndexOutOfBoundsException : public FSException
{

public:

	/**
	 * Creates a new ArrayIndexOutOfBoundsException with a default error message.
	 *
	 * @param invalidIndex the invalid index value which caused
     	 *	this exception.
	 */
	ArrayIndexOutOfBoundsException(long invalidIndex);
    
	/**
	 * Creates a new ArrayIndexOutOfBoundsException with a custom error message.
	 *
	 * @param message the custom error message.
	 * @param invalidIndex the invalid index value which caused
	 *	this exception.
	 */
	ArrayIndexOutOfBoundsException(const char* message, long invalidIndex);
    
	/**
	 * Returns the invalid index.
	 *
	 * @return the invalid index.
	 */
	long getInvalidIndex();

private:
	long index;
};

#endif
