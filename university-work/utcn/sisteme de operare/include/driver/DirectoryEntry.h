// Project: os file system
// Date: Oct 27, 2002
// Author: bogdan
// File name: DirectoryEntry.h
// Description: ...

#ifndef _DIRECTORY_ENTRY_H
#define _DIRECTORY_ENTRY_H

#include <string.h>
#include <iostream.h>
#include "../defs.h"

class DirectoryEntry
{

public:
	/**
	 * Builds a new DirectoryEntry object using default values
	 * for its members.
	 */
	DirectoryEntry();
	
	/**
	 * Builds a new DirectoryEntry object using the specified values
	 * for its members.
	 */
	DirectoryEntry(char* entryName, int entrySize, int entryType = NORMAL_FILE_TYPE);

	/**
	 * Sets the entryName data member to <code>entryName</code>.
	 */
	void setEntryName(char* entryName);

	/**
	 * Sets the entrySize data member to <code>entrySize</code>.
	 */
	void setEntrySize(int entrySize);

	/**
	 * Sets the entryType data member to <code>entryType</code>.
	 */
	void setEntryType(int entryType);
	
	/**
	 * Returns the value of the entryName data member.
	 */
	char* getEntryName();

	/**
	 * Returns the value of the entrySize data member.
	 */
	int getEntrySize();

	/**
	 * Returns the value of the entryType data member.
	 */
	int getEntryType();

protected:
	char* entryName;	// the name of the file or directory
	int entrySize;		// the size of the file or directory
	int entryType;		// the type (see & use defs.h for consistency)
};

#endif
