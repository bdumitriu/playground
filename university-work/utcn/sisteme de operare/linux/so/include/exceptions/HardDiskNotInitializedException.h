/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the interface of the HardDiskNotInitializedException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _HARD_DISK_NOT_INITIALIZED_EXCEPTION_H
#define _HARD_DISK_NOT_INITIALIZED_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised when somebody tries
 * to access the hard disk without its being successfully initialized first.
 *
 * @short Exception thrown whenever access to an uninitialized hard disk is tried.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class HardDiskNotInitializedException : public FSException
{

public:
	/**
	 * Creates a new HardDiskNotInitializedException with a default error message.
	 */
	HardDiskNotInitializedException();

	/**
	 * Creates a new HardDiskNotInitializedException with a custom error message.
	 *
	 * @param message the custom error message.
	 */
	HardDiskNotInitializedException(const char* message);
};

#endif
