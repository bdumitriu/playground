/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 11 2002
 * Description:		This is the implementation of the DirectoryEntry class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
 
#include "../include/driver/DirectoryEntry.h"

DirectoryEntry::DirectoryEntry(Inode* inode, unsigned long inodeNr)
{
	entryType = inode->getFileType();
	entryName = new char[inode->getFileNameSize()+1];
	if (entryName == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memcpy(entryName, inode->getFileName(), inode->getFileNameSize());
	entryName[inode->getFileNameSize()] = '\0';
	entrySizeBy = inode->getFileSizeInBytes();
	entrySizeBl = inode->getFileSizeInBlocks();
	crDate = new Date(*(inode->getCreationDate()));
	laDate = new Date(*(inode->getLastAccessDate()));
	luDate = new Date(*(inode->getLastUpdateDate()));
	inodeNumber = inodeNr;
}

DirectoryEntry::~DirectoryEntry()
{
	if (entryName != NULL)
		delete [] entryName;
	if (crDate != NULL)
		delete crDate;
	if (laDate != NULL)
		delete laDate;
	if (luDate != NULL)
		delete luDate;
}

unsigned char DirectoryEntry::getEntryType()
{
	return entryType;
}

char* DirectoryEntry::getEntryName()
{
	return entryName;
}

unsigned long DirectoryEntry::getEntrySizeInBytes()
{
	return entrySizeBy;
}

unsigned long DirectoryEntry::getEntrySizeInBlocks()
{
	return entrySizeBl;
}

Date* DirectoryEntry::getEntryCreationDate()
{
	return new Date(*crDate);
}

Date* DirectoryEntry::getEntryLastAccessDate()
{
	return new Date(*laDate);
}

Date* DirectoryEntry::getEntryLastUpdateDate()
{
	return new Date(*luDate);
}

unsigned long DirectoryEntry::getInodeNumber()
{
	return inodeNumber;
}
