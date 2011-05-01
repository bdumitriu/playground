// Project: os file system
// Date: Oct 27, 2002
// Author: bogdan
// File name: DirectoryListing.h
// Description: ...

#ifndef _DIRECTORY_LISTING_H
#define _DIRECTORY_LISTING_H

#include "../exceptions/ArrayIndexOutOfBoundsException.h"

class DirectoryListing
{

public:
	/**
	 * Returns the number of entries in the directory.
	 * @return the number of entries in the directory.
	 */
	virtual int size() = 0;

	/**
	 * Returns the entry in the <code>index</code>th position
	 * of the directory.
	 * @param index the index of the entry to be returned.
	 * @return the entry in the <code>index</code>th position
	 *	of the directory.
	 * @throw ArrayIndexOutOfBoundsException if index is less than
	 *	0 or greater than or equal to this->size().
	 */
	virtual DirectoryEntry* getEntry(int index)
		throw(ArrayIndexOutOfBoundsException) = 0;
};

#endif
