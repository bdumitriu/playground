/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the interface of the IOException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _IO_EXCEPTION_H
#define _IO_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever the hard disk
 * encounters an input/output problem during read/write operations.
 *
 * @short Exception thrown whenever problems are encountered during read/write operations.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class IOException : public FSException
{

public:

	/**
	 * Creates a new IOException with a default error message.
	 */
	IOException();

	/**
	 * Creates a new IOException with a custom error message.
	 *
	 * @param message the custom error message.
	 */
	IOException(const char* message);
};

#endif
