/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 3 2002
 * Description:		This is the interface of the InvalidHDDFileSizeException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _INVALID_HDD_FILE_SIZE_EXCEPTION_H
#define _INVALID_HDD_FILE_SIZE_EXCEPTION_H

#include "FSException.h"
#include "IOException.h"

/**
 * This class represents an exception that is raised whenever the system
 * detects that the size of the hard disk file does not correspond to the
 * product between (include/defs.h::NR_OF_BLOCKS) and (include/defs.h::BLOCK_DIM)
 * constants.
 *
 * @short Exception thrown whenever the hard disk file size is incorrect.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class InvalidHDDFileSizeException : public IOException
{

public:
	/**
	 * Creates a new InvalidHDDFileSizeException with a default error message.
	 *
	 * @param invalidSize the invalid file size.
	 */
	InvalidHDDFileSizeException(unsigned long invalidSize);

	/**
	 * Creates a new InvalidHDDFileSizeException with a custom error message.
	 *
	 * @param message the custom error message.
	 * @param invalidSize the invalid file size.
	 */
	InvalidHDDFileSizeException(const char* message, unsigned long invalidSize);

	/**
	 * Returns the invalid file size.
	 *
	 * @return the invalid file size.
	 */
	unsigned long getInvalidSize();

private:
	unsigned long size;
};

#endif
