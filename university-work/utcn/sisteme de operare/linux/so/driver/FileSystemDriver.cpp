/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Tue Nov 12 2002
 * Description:		This is the implementation of the FileSystemDriver class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/FileSystemDriver.h"

FileSystemDriver::FileSystemDriver()
{
	initialized = false;
}

FileSystemDriver::~FileSystemDriver()
{
	if (blockMap != NULL)
		delete blockMap;
	if (inodeMap != NULL)
	{
		inodeMap->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR-1);
		delete inodeMap;
	}
	if (hdd != NULL)
		delete hdd;
}

unsigned char FileSystemDriver::initializeDriver()
	throw(IOException*)
{
	// if it's already initialized
	if (initialized)
	{
		return 0;
	}

	try
	{
		/* *** check restrictions to be implemented here *** */

		HDDDriver* tmpHdd = new HDDDriver();
		tmpHdd->initializeHDD();
		hdd = new HDDDriverCache(tmpHdd);

		Superblock* sb = new Superblock();
		sb->readFromHardDisk(hdd);
		unsigned char* sig = new unsigned char[12];
		if (sig == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
		memcpy(sig, "bdumitriusfs", 12);

		if (memcmp(sig, sb->getSignature(), 12))
		{
			hddFormatted = false;
		}
		else
		{
			hddFormatted = true;

			inodeMap = new InodeMap();
			inodeMap->readFromHardDisk(hdd, FILEATTR_AREA_START_ADDR-1);

			blockMap = new BlockMap(hdd);
		}

		if (sig != NULL)
			delete [] sig;

		initialized = true;
		return 0;
	}
	catch (FileNotFoundException* e)
	{
		return HARD_DISK_MISSING;
	}
	catch (InvalidHDDFileSizeException* e)
	{
		return HARD_DISK_CORRUPTED;
	}
	catch (InvalidBlockNumberException* e)
	{
		return HARD_DISK_CORRUPTED;
	}
}

	/**
	 * Creates a new empty file having the specified filename and type.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @param fileName the name of the file/directory to be created.
	 * @param fileType discriminates between ordinary files, folders & other types of files
	 *	(see & use file type definitions in defs.h for type consistency).
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
unsigned char FileSystemDriver::createFile(const char* fileName, unsigned char fileType)
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	if (!hddFormatted)
	{
		return HARD_DISK_NOT_FORMATTED;
	}

	try
	{
		unsigned long length = strlen(fileName);
		if (length == 0)
		{
			return INVALID_FILE_NAME;
		}

		if (fileName[0] != PATH_SEPARATOR)
		{
			return NOT_AN_ABSOLUTE_PATH;
		}

		char* name;
		DirectoryListing* dl;
		if (fileType == NORMAL_FILE_TYPE)
		{
			dl = Directory::traversePathToFile(hdd, fileName, &name);
		}
		else
		{
			dl = Directory::traversePathToDir(hdd, fileName, &name);
		}

		if (dl == NULL)
		{
			return INVALID_SOURCE_PATH;
		}

		if (strlen(name) == 0)
		{
			return INVALID_FILE_NAME;
		}

		if (dl->contains(name))
		{
			return DUPLICATE_FILE_NAME;
		}

		unsigned long iNo = inodeMap->getFreeBit();

		if (iNo == BLOCK_DIM-20)
		{
			return NO_INODE_AVAILABLE;
		}

		Inode* inode = new Inode();
		inode->setValid(true);
		inode->setFileName(name, strlen(name));
		inode->setFileNameSize(strlen(name));
		inode->setFileSizeInBlocks(0);
		inode->setFileSizeInBytes(0);
		inode->setFileType(fileType);
		inode->setCreationDate(new Date());
		inode->setLastAccessDate(new Date());
		inode->setLastUpdateDate(new Date());

		FileOutputStream* fos;

		if (fileType == FOLDER_FILE_TYPE)
		{
			DirectoryListing* dirL = new DirectoryListing(2);
			dirL->addEntry(new DirectoryEntry(inode, iNo));
			dirL->addEntry(new DirectoryEntry(inode,
				dl->getEntry((unsigned long) 0)->getInodeNumber()));

			fos = new FileOutputStream(hdd, inode, FILEATTR_AREA_START_ADDR+iNo, blockMap);
			Directory::writeDirectory(fos, dirL);
		}

		dl->addEntry(new DirectoryEntry(inode, iNo));

		Inode* parent = new Inode();
		parent->readFromHardDisk(hdd,
			FILEATTR_AREA_START_ADDR+dl->getEntry((unsigned long) 0)->getInodeNumber());
		fos = new FileOutputStream(hdd, parent,
			FILEATTR_AREA_START_ADDR+dl->getEntry((unsigned long) 0)->getInodeNumber(), blockMap);

		try
		{
			Directory::writeDirectory(fos, dl);
		}
		catch (NotEnoughSpaceException* e)
		{
			// blockMap->freeBlocks(inode);
			inode->setValid(false);
			inode->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR+iNo);
			return HARD_DISK_FULL;
		}
		catch (FileSizeTooLargeException* e)
		{
			// blockMap->freeBlocks(inode);
			inode->setValid(false);
			inode->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR+iNo);
			return FILE_SIZE_TOO_LARGE;
		}

		inode->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR+iNo);

		inodeMap->allocInode(iNo);
		inodeMap->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR-1);

		if (name != NULL)
			delete [] name;

		return 0;
	}
	catch (InvalidBlockNumberException* e)
	{
        	return HARD_DISK_CORRUPTED;
	}
	catch (ArrayIndexOutOfBoundsException* e)
	{
        	return HARD_DISK_CORRUPTED;
	}
	catch (FileSizeTooLargeException* e)
	{
		return FILE_SIZE_TOO_LARGE;
	}
	catch (NotEnoughSpaceException* e)
	{
		return HARD_DISK_FULL;
	}
}

	/**
	 * Reads a number of <code>noBytes</code> bytes from the specified file
	 * starting from the specified offset into the <code>inBuffer</code> buffer.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @param fileName the name of the file to read from.
	 * @param offset the offset in the file where to start reading from.
	 * @param inBuffer the buffer in which to insert the read data.
	 * @param noBytes the number of bytes to read.
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
unsigned char FileSystemDriver::readBytesFromFile(const char* fileName, long offset, void* inBuffer, long noBytes)
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	if (!hddFormatted)
	{
		return HARD_DISK_NOT_FORMATTED;
	}
}

	/**
	 * Writes a number of <code>noBytes</code> bytes to the specified file
	 * starting from the specified offset of the <code>outBuffer</code> buffer.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @param fileName the name of the file to write to.
	 * @param offset the offset in the file where to start writing from.
	 * @param outBuffer the buffer containing the data to be written.
	 * @param noBytes the number of bytes to write.
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
unsigned char FileSystemDriver::writeBytesInFile(const char* fileName, long offset, void* outBuffer, long noBytes)
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	if (!hddFormatted)
	{
		return HARD_DISK_NOT_FORMATTED;
	}
}

	/**
	 * Deletes the file having the specified name. The name must represent
	 * the absolute path if directories are supported. Also, if fileName
	 * denotes a directory, it has to be empty in order to be deleted.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @param fileName the name of the file to be deleted.
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
unsigned char FileSystemDriver::deleteFile(const char* fileName)
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	if (!hddFormatted)
	{
		return HARD_DISK_NOT_FORMATTED;
	}
}

	/**
	 * Renames the file having the name specified by the first argument with
	 * the name specified by the second argument.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @param oldFileName the original file name.
	 * @param newFileName the new file name.
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
unsigned char FileSystemDriver::renameFile(const char* oldFileName, const char* newFileName)
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	if (!hddFormatted)
	{
		return HARD_DISK_NOT_FORMATTED;
	}
}

	/**
	 * Returns the size of the file specified by <code>fileName</code>
	 * in the <code>fileSize</code> parameter. If anything goes wrong,
	 * <code>fileSize</code> will be set to 0.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @param fileName the name of the file whose size you want.
	 * @param fileSize the output parameter which will contain the file size.
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
unsigned char FileSystemDriver::getFileSize(const char* fileName, unsigned long* fileSize)
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	if (!hddFormatted)
	{
		return HARD_DISK_NOT_FORMATTED;
	}
}

unsigned char FileSystemDriver::listFiles(const char* directoryName, DirectoryListing** dirListing)
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	if (!hddFormatted)
	{
		return HARD_DISK_NOT_FORMATTED;
	}

	try
	{
		unsigned long length = strlen(directoryName);
		if (length == 0)
		{
			return INVALID_FILE_NAME;
		}
		if (directoryName[0] != PATH_SEPARATOR)
		{
			return NOT_AN_ABSOLUTE_PATH;
		}
		if (directoryName[length-1] != PATH_SEPARATOR)
		{
			char* newDirName = new char[length+2];
			if (newDirName == NULL)
			{
				cout << "Not enough memory.\n";
				exit(1);
			}
		
			strcpy(newDirName, directoryName);
			newDirName[length] = PATH_SEPARATOR;
			newDirName[length+1] = '\0';

			char* fileName;
			(*dirListing) = Directory::traversePathToFile(hdd, newDirName, &fileName);
			if (*dirListing == NULL)
			{
				return INVALID_SOURCE_PATH;
			}
			if (fileName != NULL)
				delete [] fileName;
		}
		else
		{
			char* fileName;
			(*dirListing) = Directory::traversePathToFile(hdd, directoryName, &fileName);
			if (*dirListing == NULL)
			{
				return INVALID_SOURCE_PATH;
			}
			if (fileName != NULL)
				delete [] fileName;

		}

		return 0;
	}
	catch (InvalidBlockNumberException* e)
	{
        	return HARD_DISK_CORRUPTED;
	}
	catch (ArrayIndexOutOfBoundsException* e)
	{
        	return HARD_DISK_CORRUPTED;
	}
}

unsigned char FileSystemDriver::formatHDD()
	throw(IOException*)
{
	if (!initialized)
	{
		return DRIVER_NOT_INITIALIZED;
	}
	try
	{
		HDDFormatter::formatHDD(hdd);

		inodeMap = new InodeMap();
		inodeMap->readFromHardDisk(hdd, FILEATTR_AREA_START_ADDR-1);

		blockMap = new BlockMap(hdd);
	
		// create the root directory
		Inode* rootInode = new Inode();
		rootInode->setValid(1);
		rootInode->setFileName("/", 1);
		rootInode->setFileNameSize(1);
		rootInode->setFileType(FOLDER_FILE_TYPE);
		rootInode->setCreationDate(new Date());
		rootInode->setLastAccessDate(new Date());
		rootInode->setLastUpdateDate(new Date());
		rootInode->setFileSizeInBlocks(0);
		rootInode->setFileSizeInBytes(0);

		DirectoryListing* dl = new DirectoryListing(2);
		dl->addEntry(new DirectoryEntry(rootInode, ROOT_DIR_FILEATTR));
		dl->addEntry(new DirectoryEntry(rootInode, ROOT_DIR_FILEATTR));

		rootInode->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR+ROOT_DIR_FILEATTR);
		Directory::writeDirectory(new FileOutputStream(hdd, rootInode,
			FILEATTR_AREA_START_ADDR+ROOT_DIR_FILEATTR, blockMap), dl);

		if (rootInode != NULL)
			delete rootInode;
		if (dl != NULL)
			delete dl;

		hddFormatted = true;

		return 0;
	}
	catch (FileSizeTooLargeException* e)
	{
		return FILE_SIZE_TOO_LARGE;
	}
	catch (NotEnoughSpaceException* e)
	{
		return HARD_DISK_FULL;
	}
	catch (InvalidBlockNumberException* e)
	{
        	return HARD_DISK_CORRUPTED;
	}
	catch (ArrayIndexOutOfBoundsException* e)
	{
        	return HARD_DISK_CORRUPTED;
	}	
}
