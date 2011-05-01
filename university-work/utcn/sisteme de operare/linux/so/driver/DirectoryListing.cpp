/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 11 2002
 * Description:		This is the implementation of the DirectoryListing class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/DirectoryListing.h"

DirectoryListing::DirectoryListing()
{
	capacity = 50;
	length = 0;
	entries = new (DirectoryEntry*)[capacity];
	if (entries == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
}

DirectoryListing::DirectoryListing(unsigned long initialCapaicty)
{
	capacity = initialCapaicty;
	length = 0;
	entries = new (DirectoryEntry*)[capacity];
	if (entries == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
}

DirectoryListing::~DirectoryListing()
{
	if (entries != NULL)
	{
		for (unsigned long i = length; i < capacity; i++)
			entries[i] = NULL;
		delete [] entries;
	}
}

unsigned long DirectoryListing::size()
{
	return length;
}

void DirectoryListing::addEntry(DirectoryEntry* entry)
{
	if (length == capacity)
	{
		capacity *= 2;
		DirectoryEntry** newEntries = new (DirectoryEntry*)[capacity];
		if (newEntries == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
		for (unsigned long i = 0; i < length; i++)
		{
			newEntries[i] = entries[i];
			entries[i] = NULL;
		}
		if (entries != NULL)
			delete [] entries;
		entries = newEntries;
	}

	entries[length++] = entry;
}

DirectoryEntry* DirectoryListing::getEntry(unsigned long index) throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= length)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}

	return entries[index];
}

DirectoryEntry* DirectoryListing::getEntry(const char* entryName)
{
	unsigned long index = indexOf(entryName);

	if (index != length)
		return entries[index];
	else
		return NULL;
}

DirectoryEntry* DirectoryListing::deleteEntry(unsigned long index) throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= length)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}

	DirectoryEntry* entry = entries[index];

	for (unsigned long i = index; i < length-1; i++)
		entries[i] = entries[i+1];

	length--;

	return entry;
}

unsigned long DirectoryListing::indexOf(const char* entryName)
{
	for (unsigned long i = 0; i < length; i++)
		if (!strcmp(entryName, entries[i]->getEntryName()))
			return i;

	return length;
}

bool DirectoryListing::contains(const char* entryName)
{
	if (indexOf(entryName) != length)
		return true;
	else
		return false;
}
