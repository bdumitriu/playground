/*
 * Project:	File System Simulator
 * Author:	Bogdan DUMITRIU
 * E-mail:	bdumitriu@bdumitriu.ro
 * Date:	Sun Oct 27 2002
 * Description:	This is the interface of the DirectoryListing class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _DIRECTORY_LISTING_H
#define _DIRECTORY_LISTING_H

#include <stdlib.h>
#include <iostream.h>
#include <string.h>
#include "DirectoryEntry.h"
#include "../exceptions/ArrayIndexOutOfBoundsException.h"

/**
 * This class contains a set of directory entries (i.e. files or (sub)directories)
 * which represent a directory's contents. It allows manipulation of this set of
 * entries.
 *
 * @short This class manages a set of directory entries.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class DirectoryListing
{

public:
	/**
	 * Creates an empty directory listing with an initial capacity
	 * of 50. This capacity represents the number of entries the
	 * internal array can store. If this capacity is reached a
	 * new array with a double capacity is allocated and the old
	 * one is copied into the new one.
	 */
	DirectoryListing();

	/**
	 * Creates an empty directory listing  with <code>initialCapacity</code>
	 * as its initial capatcity. This capacity represents the number of
	 * entries the internal array can store. If this capacity is reached a
	 * new array with a double capacity is allocated and the old
	 * one is copied into the new one.
	 */
	DirectoryListing(unsigned long initialCapaicty);

	/**
	 * The destructor of the class. This destructor deletes the
	 * internal array and all the DirectoryEntry objects as
	 * well.
	 */
	~DirectoryListing();

	/**
	 * Returns the number of entries in the directory listing.
	 *
	 * @return the number of entries in the directory listing.
	 */
	unsigned long size();

	/**
	 * Adds the entry received as parameter in the listing.
	 *
	 * @param entry the entry too add.
	 */
	void addEntry(DirectoryEntry* entry);

	/**
	 * Returns the entry in the <code>index</code>th position
	 * of the directory.
	 *
	 * The method throws:
	 * <ul>
	 * <li>	ArrayIndexOutOfBoundsException* if <code>index</code>
	 * is less than 0 or greater than or equal to <code>this->size()</code>.
	 * </li>
	 * </ul>
	 *
	 * @param index the index of the entry to be returned.
	 * @return the entry in the <code>index</code>th position
	 *	of the directory.
	 */
	DirectoryEntry* getEntry(unsigned long index) throw(ArrayIndexOutOfBoundsException*);

        /**
	 * Returns the DirectoryEntry whose entry name is equal to
	 * <code>entryName</code>. If no such entry exists, NULL
	 * is returned.
	 *
	 * @param entryName the entry name of the entry you want. It
	 *	should be a string (i.e., it should end with '\0').
	 * @return the DirectoryEntry whose entry name is equal to
	 *	<code>entryName</code>.
	 */
	DirectoryEntry* getEntry(const char* entryName);

	/**
	 * Delete the <code>index</code>th entry in the listing
	 * and shifts the entries that follow one position to
	 * the left.
	 *
	 * The method throws:
	 * <ul>
	 * <li>	ArrayIndexOutOfBoundsException* if <code>index</code>
	 * is less than 0 or greater than or equal to <code>this->size()</code>.
	 * </li>
	 * </ul>
	 *
	 * @param index the index of the entry to be deleted.
	 * @return the deleted DirectoryEntry.
	 */
	DirectoryEntry* deleteEntry(unsigned long index) throw(ArrayIndexOutOfBoundsException*);

        /**
	 * Returns the index of the DirectoryEntry whose entry name is
	 * equal to <code>entryName</code>. If no such entry exists, this->size()
	 * is returned.
	 *
	 * @param entryName the entry name of the entry whose index you want. It
	 *	should be a string (i.e., it should end with '\0').
	 * @return the index of the DirectoryEntry whose entry name is
	 *	equal to <code>entryName</code>.
	 */
	unsigned long indexOf(const char* entryName);

	/**
	 * Returns true if listing contains an entry with <code>entryName</code>
	 * as its entry name and false otherwise.
	 *
	 * @param entryName the entry name of the entry you are looking for It
	 *	should be a string (i.e., it should end with '\0').
	 * @return true if listing contains an entry with <code>entryName</code>
	 * as its entry name and false otherwise.
	 */
	bool contains(const char* entryName);
	
private:
	DirectoryEntry** entries;	/* the internal array */
	unsigned long length;		/* number of valid elements in the array */
	unsigned long capacity;		/* the number of elements the array can hold */
};

#endif
