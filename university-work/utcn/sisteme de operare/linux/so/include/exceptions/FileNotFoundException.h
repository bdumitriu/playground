/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 3 2002
 * Description:		This is the interface of the FileNotFoundException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _FILE_NOT_FOUND_EXCEPTION_H
#define _FILE_NOT_FOUND_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever somebody tries
 * to open a file which cannot be found on the disk.
 *
 * @short Exception thrown whenever an access to an inexistent file is tried.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class FileNotFoundException : public FSException
{

public:
	/**
	 * Creates a new FileNotFoundException with a default error message.
	 *
	 * @param invalidFileName the name of the file which could not be found.
	 */
	FileNotFoundException(const char* invalidFileName);

	/**
	 * Creates a new FileNotFoundException with a custom error message.
	 *
	 * @param message the custom error message.
	 * @param invalidFileName the name of the file which could not be found.
	 */
	FileNotFoundException(const char* message, const char* invalidFileName);

	/**
	 * The destructor of the class.
	 */
	~FileNotFoundException();

	/**
	 * Returns the name of the file which could not be found.
	 *
	 * @return the name of the file which could not be found.
	 */
	char* getInvalidFileName();

private:
	char* fileName;
};

#endif
