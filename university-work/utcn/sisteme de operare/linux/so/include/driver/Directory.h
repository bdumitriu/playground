/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 11 2002
 * Description:		This is the interface of the Directory class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _DIRECTORY_H
#define _DIRECTORY_H

#include <stdlib.h>
#include <iostream.h>
#include <string.h>
#include "HDD.h"
#include "FileInputStream.h"
#include "FileOutputStream.h"
#include "Inode.h"
#include "DirectoryEntry.h"
#include "DirectoryListing.h"
#include "../utils/Utils.h"
#include "../utils/PathParser.h"
#include "../exceptions/HardDiskNotInitializedException.h"
#include "../exceptions/InvalidBlockNumberException.h"
#include "../exceptions/InvalidDateException.h"
#include "../exceptions/IOException.h"

/**
 * This class allows the user to read/write a directory's contents.
 * Technically, it makes the directory encoding manner transparent.The
 * directory's contents can be obtained in the form of a DirectoryListing
 * object and can be written from an array of unsigned longs.
 *
 * This is the configuration of a directory this class assumes:
 * <ul>
 * <li> first 4 bytes - number of entries (unsigned long) including "." & "..".</li>
 * <li> next 4 bytes - "." directory absolute inode number (pointer
 *	to own inode).</li>
 * <li> next 4 bytes - ".." directory absolute inode number (pointer
 *	to parent directory inode).</li>
 * <li> 4 bytes - absolute inode number of each of the other entries.</li>
 * </ul>
 *
 * @short This class allows easy reading/writing of a directory's contents.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class Directory
{
public:
	/**
	 * Reads the directory from the disk and returns the directory
	 * listing which encapsulates the directory's contents.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * <li> ArrayIndexOutOfBoundsException* forwarded form <code>fis</code>.</li>
	 * </ul>
	 *
	 * @param hdd the hard disk used for reading inodes.
	 * @param fis the FileInputStream associated with the directory.
	 *	inode.
	 * @return directory listing which encapsulates the directory's contents.
	 */
	/*
	 * the method should also throw a BadDirectoryException* when the directory
	 * structure on the hard disk is incorrect, but this is not implemented yet.
	 */
	static DirectoryListing* readDirectory(HDD* hdd, FileInputStream* fis) throw(
		IOException*,
		InvalidBlockNumberException*,
		HardDiskNotInitializedException*,
		ArrayIndexOutOfBoundsException*);

	/**
	 * Writes <code>size</code> entries from the <code>contents</code> array to
	 * the hard disk using the <code>fos</code> FileOutputStream - an output stream
	 * opened using the directory's inode.
	 *
	 * The method throws:
	 * <ul>
	 * <li> NotEnoughSpaceException* forwarded from <code>fos</code>.</li>
	 * <li> FileSizeTooLargeException* forwarded from <code>fos</code>.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>fos</code>.</li>
	 * <li> IOException* forwarded from <code>fos</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>fos</code>.</li>
	 * <li> ArrayIndexOutOfBoundsException* forwarded form <code>fos</code>.</li>
	 * </ul>
	 *
	 * @param fos the FileOutputStream associated with the directory.
	 * @param dirListing a DirectoryListing object representing the diretory contents
	 *	to be written on disk (the first element should be "." and the second one
	 *	"..").
	 */
	static void writeDirectory(FileOutputStream* fos, DirectoryListing* dirListing) throw(
		NotEnoughSpaceException*,
		FileSizeTooLargeException*,
		IOException*,
		InvalidBlockNumberException*,
		HardDiskNotInitializedException*,
		ArrayIndexOutOfBoundsException*);

	/**
	 * This method takes the absolute path <code>path</code> as parameter and
	 * returns the diectory listing of the last directory in the path. <code>fileName</code>
	 * is an output parameter and it will contain the name of the file from the path.
	 * For example, if path is /usr/local/bin/opera then the directory listing will be
	 * that of the /usr/local/bin directory and the file name will be opera.
	 *
	 * If, on the other hand, you provide the method with a path like /usr/local/bin/
	 * it will return the directory listing of /usr/local/bin and fileName will be	
	 * the empty string.
	 *
	 * If the path is invalid, the method returns NULL.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>readDirectory</code>.</li>
	 * <li> IOException* forwarded from <code>readDirectory</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>readDirectory</code>.</li>
	 * <li> ArrayIndexOutOfBoundsException* forwarded form <code>readDirectory</code>.</li>
	 * </ul>
	 *
	 * @param hdd the hard disk used for reading inodes.
	 * @param path the path to traverse.
	 * @param fileName output parameter in which the name of the file will be stored.
	 * @return the directory listing of the last directory in the path.
	 */
	static DirectoryListing* traversePathToFile(HDD* hdd, const char* path, char** fileName) throw(
		IOException*,
		InvalidBlockNumberException*,
		HardDiskNotInitializedException*,
		ArrayIndexOutOfBoundsException*);

	/**
	 * This method takes the absolute path <code>path</code> as parameter and
	 * returns the diectory listing of the first to last directory in the path.
	 * <code>dirName</code> is an output parameter and it will contain the name
	 *  of the last directory from the path. In order to see the difference between
	 * this method and the traversePathToFile() method, here's an example for easy
	 * understandig: in this case, both a path like /usr/local/bin and a path like
	 * /usr/local/bin/ will cause the same result - the directory listing will be
	 * that of the /usr/local directory and <code>dirName</code> will be bin.
	 *
	 * If the path is invalid, the method returns NULL.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>readDirectory</code>.</li>
	 * <li> IOException* forwarded from <code>readDirectory</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>readDirectory</code>.</li>
	 * <li> ArrayIndexOutOfBoundsException* forwarded form <code>readDirectory</code>.</li>
	 * </ul>
	 *
	 * @param hdd the hard disk used for reading inodes.
	 * @param path the path to traverse.
	 * @param dirName output parameter in which the name of the directory will be stored.
	 * @return the directory listing of the first last directory in the path.
	 */
	static DirectoryListing* traversePathToDir(HDD* hdd, const char* path, char** dirName) throw(
		IOException*,
		InvalidBlockNumberException*,
		HardDiskNotInitializedException*,
		ArrayIndexOutOfBoundsException*);
};

#endif
