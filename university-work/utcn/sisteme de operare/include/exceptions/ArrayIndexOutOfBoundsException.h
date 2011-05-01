// Project: os file system
// Creation date: Oct 27, 2002
// Author: bogdan
// File name: ArrayIndexOutOfBoundsException.h
// Description: ...

#ifndef _ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION_H
#define _ARRAY_INDEX_OUT_OF_BOUNDS_EXCEPTION_H

#include <strstream.h>

class ArrayIndexOutOfBoundsException
{

public:
	/**
	 * Builds a new ArrayIndexOutOfBoundsException.
	 * @param outOfBoundsIndex the illegal index value which caused
	 *	this exception.
	 */
	ArrayIndexOutOfBoundsException(int outOfBoundsIndex);

	/**
	 * Returns a string containing a description of this exception.
	 * @return a string containing a description of this exception.
	 */
	char* getMessage();

private:
	int index;
};

#endif
