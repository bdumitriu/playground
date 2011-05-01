// Project: os file system
// Creation date: Oct 27, 2002
// Author: bogdan
// File name: DirectoryEntry.cpp
// Description: ...

#include "../include/driver/DirectoryEntry.h"

DirectoryEntry::DirectoryEntry()
: entrySize(0), entryType(0)
{
	this->entryName = new char[0];
	if (this->entryName == 0)
	{
		cerr << "Not enough memory";
		exit(1);
	}
	strcpy(this->entryName, "");
}

DirectoryEntry::DirectoryEntry(char* entryName, int entrySize, int entryType)
: entrySize(entrySize), entryType(entryType)
{
	this->entryName = new char[strlen(entryName)];
	if (this->entryName == 0)
	{
		cout << "Not enough memory";
		exit(1);
	}
	strcpy(this->entryName, entryName);
}

void DirectoryEntry::setEntryName(char* entryName)
{
	delete this->entryName;
	this->entryName = new char[strlen(entryName)];
	if (this->entryName == 0)
	{
		cout << "Not enough memory";
		exit(1);
	}
	strcpy(this->entryName, entryName);
}

void DirectoryEntry::setEntrySize(int entrySize)
{
	this->entrySize = entrySize;
}

void DirectoryEntry::setEntryType(int entryType)
{
	this->entryType = entryType;
}

char* DirectoryEntry::getEntryName()
{
	return entryName;
}

int DirectoryEntry::getEntrySize()
{
	return entrySize;
}

int DirectoryEntry::getEntryType()
{
	return entryType;
}
