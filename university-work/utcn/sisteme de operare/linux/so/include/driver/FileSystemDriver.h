/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Tue Nov 12 2002
 * Description:		This is the interface of the FileSystemDriver class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _FILE_SYSTEM_DRIVER_H
#define _FILE_SYSTEM_DRIVER_H

#include <stdlib.h>
#include <iostream.h>
#include <string.h>

#include "../defs.h"
#include "../utils/Utils.h"
#include "../utils/Date.h"

#include "HDD.h"
#include "HDDDriver.h"
#include "HDDDriverCache.h"

#include "HDDFormatter.h"

#include "Superblock.h"
#include "Inode.h"
#include "InodeMap.h"
#include "BlockMap.h"

#include "FileInputStream.h"
#include "FileOutputStream.h"

#include "DirectoryEntry.h"
#include "DirectoryListing.h"
#include "Directory.h"

/**
 * This class represents the primary interface to the file system. It
 * should be the only one the user should interact with. It provides
 * complete access to the file system functionality.
 *
 * @short the primary interface to the file system.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class FileSystemDriver
{
public:
        /**
	 * Creates a new uninitialized FileSystemDriver.
	 */
	FileSystemDriver();

	/**
	 * The destructor of the class.
	 */
	~FileSystemDriver();

	/**
	 * Initializes the internal structures of the driver. No other
	 * method can be run successfully before this method is
	 * run successfully.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
	unsigned char initializeDriver()
		throw(IOException*);

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
	unsigned char createFile(const char* fileName, unsigned char fileType)
 		throw(IOException*);

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
	unsigned char readBytesFromFile(const char* fileName, long offset, void* inBuffer, long noBytes)
 		throw(IOException*);

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
	unsigned char writeBytesInFile(const char* fileName, long offset, void* outBuffer, long noBytes)
 		throw(IOException*);

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
	unsigned char deleteFile(const char* fileName)
 		throw(IOException*);

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
	unsigned char renameFile(const char* oldFileName, const char* newFileName)
 		throw(IOException*);

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
	unsigned char getFileSize(const char* fileName, unsigned long* fileSize)
		throw(IOException*);

	/**
	 * Returns a DirectoryListing object containing data about all the
	 * files in the <code>directoryName</code> directory in the
	 * <code>dirListing</code> parameter. If anything goes wrong,
	 * <code>dirListing</code> will be set to NULL.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @param directoryName the name of the directory whose contents
	 *	you want.
	 * @param dirListing a pointer to a DirectoryListing* where the
	 *	DirectoryListing will be stored.
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
    	 */
	 unsigned char listFiles(const char* directoryName, DirectoryListing** dirListing)
		throw(IOException*);

	/**
	 * Formats the hard disk drive. This function formats the special
	 * hard disk structures in order to support the filesystem.
	 *
	 * The method throws:
	 * <ul>
	 * <li> IOException* if an I/O error occured during operation.</li>
	 * </ul>
	 *
	 * @return 0 if operation was successful / error code otherwise. The error
	 *	codes can be found in include/defs.h.
	 */
	unsigned char formatHDD()
 		throw(IOException*);

private:
	BlockMap* blockMap;
	InodeMap* inodeMap;
	HDD* hdd;
	bool hddFormatted;
	bool initialized;
};

#endif
