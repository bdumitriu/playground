/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 3 2002
 * Description:		This is the interface of the InvalidBlockNumberException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _INVALID_BLOCK_NUMBER_EXCEPTION_H
#define _INVALID_BLOCK_NUMBER_EXCEPTION_H

#include "FSException.h"

/**
 * This class represents an exception that is raised whenever somebody tries
 * to access a block which does not actually exist (i.e. the block index is
 * either a negative value or a value grater than or equal to
 * include/defs.h::NR_OF_BLOCKS).
 *
 * @short Exception thrown whenever an access to an invalid block is tried.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class InvalidBlockNumberException : public FSException
{

public:
	/**
	 * Creates a new InvalidBlockNumberException with a default error message.
	 *
	 * @param invalidBlockNo the invalid block number.
	 */
	InvalidBlockNumberException(long invalidBlockNo);

	/**
	 * Creates a new InvalidBlockNumberException with a custom error message.
	 *
	 * @param message the custom error message.
	 * @param invalidBlockNo the invalid block number.
	 */
	InvalidBlockNumberException(const char* message, long invalidBlockNo);

	/**
	 * Returns the invalid block number.
	 *
	 * @return the invalid block number.
	 */
	long getInvalidBlockNo();

private:
	long blockNo;
};

#endif
