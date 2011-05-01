/*
 * Project:	File System Simulator
 * Author:	Bogdan DUMITRIU
 * E-mail:	bdumitriu@bdumitriu.ro
 * Date:	Mon Nov 11 2002
 * Description:	This is the interface of the DirectoryEntry class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _DIRECTORY_ENTRY_H
#define _DIRECTORY_ENTRY_H

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include "Inode.h"
#include "../utils/Date.h"
#include "../defs.h"

/**
 * This class encapsulates data pertaining to a directory entry (normally,
 * a file or a (sub)directory). It provides a constructor which creates the
 * entry using the information found in an inode and getters for all of
 * the elements.
 *
 * @short This class encapsulates data pertaining to a directory entry.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class DirectoryEntry
{

public:
	/**
	 * Creates a new DirectoryEntry and populates its data members
	 * using the following values from the Inode object:
	 *
	 * <ul>
	 * <li> the file type.</li>
	 * <li> the file name (and for this, the file name size...).</li>
	 * <li> the file size in bytes.</li>
	 * <li> the file size in blocks.</li>
	 * <li> the file creation date.</li>
	 * <li> the file last access date.</li>
	 * <li> the file last update date.</li>
	 * </ul>
	 *
	 * It assumes that <code>inode</code> contains valid information.
	 *
	 * @param inode the valid inode containing the required information.
	 * @param inodeNumber the actual number of this inode.
	 */
	DirectoryEntry(Inode* inode, unsigned long inodeNumber);

	/**
	 * The destructor of the class.
	 */
	~DirectoryEntry();

	/**
	 * Returns the value of the entryType data member.
	 *
	 * @return the value of the entryType data member.
	 */
	unsigned char getEntryType();

	/**
	 * Returns the value of the entryName data member.
	 *
	 * @return the value of the entryName data member.
	 */
	char* getEntryName();

	/**
	 * Returns the value of the entrySizeInBytes data member.
	 *
	 * @return the value of the entrySizeInBytes data member.
	 */
	unsigned long getEntrySizeInBytes();

	/**
	 * Returns the value of the entrySizeInBlocks data member.
	 *
	 * @return the value of the entrySizeInBlocks data member.
	 */
	unsigned long getEntrySizeInBlocks();

	/**
	 * Returns the value of the entryCreationDate data member.
	 *
	 * @return the value of the entryCreationDate data member.
	 */
	Date* getEntryCreationDate();

	/**
	 * Returns the value of the entryLastAccessDate data member.
	 *
	 * @return the value of the entryLastAccessDate data member.
	 */
	Date* getEntryLastAccessDate();

	/**
	 * Returns the value of the entryLastUpdateDate data member.
	 *
	 * @return the value of the entryLastUpdateDate data member.
	 */
	Date* getEntryLastUpdateDate();

	/**
	 * Returns the value of the inodeNumber data member.
	 *
	 * @return the value of the inodeNumber data member.
	 */
	unsigned long getInodeNumber();

private:
	unsigned char entryType;	/* the type (see & use include/defs.h for consistency) */
	char* entryName;		/* the name of the file or directory */	
	unsigned long entrySizeBy;	/* the size (in bytes) of the file or directory */
	unsigned long entrySizeBl;	/* the size (in blocks) of the file or directory */
	Date* crDate;			/* the creation date of the file */
	Date* laDate;			/* the last access date of the file */
	Date* luDate;			/* the last update date of the file */
	unsigned long inodeNumber;	/* the number of the inode associated to this entry */
};

#endif
