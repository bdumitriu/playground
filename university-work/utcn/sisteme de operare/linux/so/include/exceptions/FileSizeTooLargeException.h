/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 9 2002
 * Description:		This is the interface of the FileSizeTooLargeException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _FILE_SIZE_TOO_LARGE_EXCEPTION_H
#define _FILE_SIZE_TOO_LARGE_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever somebody
 * tries to write a file with more than the maximum number of bytes which
 * can be accomodated using the inode addressing scheme.
 *
 * @short Exception thrown whenever the inode structure is too small to hold too large a file.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class FileSizeTooLargeException : public FSException
{

public:

	/**
	 * Creates a new FileSizeTooLargeException with a default error message.
	 */
	FileSizeTooLargeException();

	/**
	 * Creates a new FileSizeTooLargeException with a custom error message.
	 *
	 * @param message the custom error message.
	 */
	FileSizeTooLargeException(const char* message);
};

#endif
