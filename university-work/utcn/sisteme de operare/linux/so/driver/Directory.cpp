/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 11 2002
 * Description:		This is the implementation of the Directory class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/Directory.h"

DirectoryListing* Directory::readDirectory(HDD* hdd, FileInputStream* fis) throw(
	IOException*,
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	ArrayIndexOutOfBoundsException*)
{
	// temporary buffer
	unsigned char* buffer = new unsigned char[4];
	if (buffer == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	fis->setPointer(0);
	fis->read(buffer, 4);
	unsigned long size = Utils::bytesToULong(buffer);
	DirectoryListing* dirList = new DirectoryListing(size);

	for (unsigned long i = 0; i < size; i++)
	{
		fis->read(buffer, 4);
		unsigned long val = Utils::bytesToULong(buffer);
		Inode* inode = new Inode();
		inode->readFromHardDisk(hdd, val);
		if (i == 0)
		{
			inode->setFileName(".", 1);
			inode->setFileNameSize(1);
		}
		if (i == 1)
		{
			inode->setFileName("..", 2);
			inode->setFileNameSize(2);
		}
		DirectoryEntry* entry = new DirectoryEntry(inode, val-FILEATTR_AREA_START_ADDR);
		dirList->addEntry(entry);
	}

	if (buffer != NULL)
		delete [] buffer;

	return dirList;
}

void Directory::writeDirectory(FileOutputStream* fos, DirectoryListing* dirList)  throw(
	NotEnoughSpaceException*,
	FileSizeTooLargeException*,
	IOException*,
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	ArrayIndexOutOfBoundsException*)
{
	// temporary buffer
	unsigned char* buffer;

	unsigned long size = dirList->size();
	fos->setPointer(0);
	buffer = Utils::uLongToBytes(size);
	fos->write(buffer, 4);

	for (unsigned long i = 0; i < size; i++)
	{
		buffer = Utils::uLongToBytes(FILEATTR_AREA_START_ADDR+dirList->getEntry(i)->getInodeNumber());
		fos->write(buffer, 4);
	}

	if (buffer != NULL)
		delete [] buffer;
}

DirectoryListing* Directory::traversePathToFile(HDD* hdd, const char* path, char** fileName) throw(
	IOException*,
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	ArrayIndexOutOfBoundsException*)
{
	PathParser* prs = new PathParser(path);

	// iNo = inode Number
	// always start with root directory
	unsigned long iNo = FILEATTR_AREA_START_ADDR+ROOT_DIR_FILEATTR;
	Inode* inode = new Inode();
	inode->readFromHardDisk(hdd, iNo);
	FileInputStream* fis = new FileInputStream(hdd, inode);
	DirectoryListing* dl = Directory::readDirectory(hdd, fis);

	if (prs->endsWithPathSeparator())
	{
		while (prs->hasMore())
		{
			char* pElem = prs->next();
			if (!dl->contains(pElem))
				return NULL;
			iNo = FILEATTR_AREA_START_ADDR+(dl->getEntry(pElem)->getInodeNumber());
			if (pElem != NULL)
				delete [] pElem;
			inode->readFromHardDisk(hdd, iNo);
			delete fis;
			fis = new FileInputStream(hdd, inode);
			delete dl;
			dl = Directory::readDirectory(hdd, fis);
		}
		(*fileName) = new char[1];
		if ((*fileName) == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
		(*fileName)[0] = '\0';
	}
	else
	{
		while (prs->hasMore())
		{
			char* pElem = prs->next();
			if (prs->hasMore())
			{
				if (!dl->contains(pElem))
					return NULL;
				iNo = FILEATTR_AREA_START_ADDR+(dl->getEntry(pElem)->getInodeNumber());
				if (pElem != NULL)
					delete [] pElem;
				inode->readFromHardDisk(hdd, iNo);
				delete fis;
				fis = new FileInputStream(hdd, inode);
				delete dl;
				dl = Directory::readDirectory(hdd, fis);
			}
			else
			{
				(*fileName) = pElem;
			}
		}
	}

	if (prs != NULL)
		delete prs;
	if (inode != NULL)
		delete inode;
	if (fis != NULL)
		delete fis;

	return dl;
}

DirectoryListing* Directory::traversePathToDir(HDD* hdd, const char* path, char** dirName) throw(
	IOException*,
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	ArrayIndexOutOfBoundsException*)
{
	PathParser* prs = new PathParser(path);

	// iNo = inode Number
	// always start with root directory
	unsigned long iNo = FILEATTR_AREA_START_ADDR+ROOT_DIR_FILEATTR;
	Inode* inode = new Inode();
	inode->readFromHardDisk(hdd, iNo);
	FileInputStream* fis = new FileInputStream(hdd, inode);
	DirectoryListing* dl = Directory::readDirectory(hdd, fis);

	(*dirName) = new char[1];
	if (dirName == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	dirName[0] = '\0';

	while (prs->hasMore())
	{
		char* pElem = prs->next();
		if (prs->hasMore())
		{
			if (!dl->contains(pElem))
				return NULL;
			iNo = FILEATTR_AREA_START_ADDR+(dl->getEntry(pElem)->getInodeNumber());
			if (pElem != NULL)
				delete [] pElem;
			inode->readFromHardDisk(hdd, iNo);
			delete fis;
			fis = new FileInputStream(hdd, inode);
			delete dl;
			dl = Directory::readDirectory(hdd, fis);
		}
		else
		{
			(*dirName) = pElem;
		}
	}

	if (prs != NULL)
		delete prs;
	if (inode != NULL)
		delete inode;
	if (fis != NULL)
		delete fis;

	return dl;
}
