// Project: os file system
// Date: Oct 18, 2002
// Author: andi
// Modifications: bogdan
// Modification date: Oct 21, 2002
// File name: FileSystemDriver.h
// Description: ...

#ifndef _FILE_SYSTEM_DRIVER_H
#define _FILE_SYSTEM_DRIVER_H

#include "IOException.h"

class FileSystemDriver
{

public:
	/*
	 * No constructor is needed as this is an "abstract" (understand it the Java way)
	 * class (or an interface, if you prefer) and a default constructor will
	 * automagically be created by the preprocessor/compiler/linker/whatever...
	 */

	/**
	 * Creates a new empty file having the specified filename and type. This method
	 * returns the error code, or 0 if no error occured.
	 * @param fileName the name of the file/directory/etc. to be created.
	 * @param fileType discriminates between ordinary files, folders & other types of files
	 * (see & use file type definitions in defs.h for type consistency).
	 * @return true if file has been successfully created / false otherwise
	 *	(typically because file already existed).
	 * @throws IOException if an I/O error occured.
	 */
	virtual bool createFile(char* fileName, int fileType)
 		throw(IOException) = 0;

	/**
	 * Reads a number of <code>noBytes</code> bytes from the specified file
	 * starting from the specified offset into the <code>inBuffer</code> buffer.
	 * @param fileName the name of the file to read from.
	 * @param offset the offset in the file where to start reading from.
	 * @param inBuffer the buffer in which to insert the read data.
	 * @param noBytes the number of bytes to read.
	 * @return the actual number of bytes read or -1 if the end of the file has been reached.
	 * @throws IOException if an I/O error occured.
	 */
	virtual long readBytesFromFile(char* fileName, long offset, void* inBuffer, long noBytes)
 		throw(IOException) = 0;

	/**
	 * Writes a number of <code>noBytes</code> bytes to the specified file
	 * starting from the specified offset of the <code>outBuffer</code> buffer.
	 * @param fileName the name of the file to write to.
	 * @param offset the offset in the file where to start writing from.
	 * @param outBuffer the buffer containing the data to be written.
	 * @param noBytes the number of bytes to write.
	 * @throws IOException if an I/O error occured.
	 */
	virtual void writeBytesInFile(char* fileName, long offset, void* outBuffer, long noBytes)
 		throw(IOException) = 0;

	/**
	 * Deletes the file having the specified name. The name must represent
	 * the absolute path if directories are supported. Also, if fileName
	 * denotes a directory, it has to be empty in order to be deleted.
	 * @param fileName the name of the file to be deleted.
	 * @return true if the deletion succeeded / false otherwise (typical
	 *	reasons: file doesn't exist, directory not empty)
	 * @throws IOException if an I/O error occured.
	 */
	virtual bool deleteFile(char* fileName)
 		throw(IOException) = 0;

	/**
	 * Renames the file having the name specified by the first argument with
	 * the name specified by the second argument.
	 * @param oldFileName the original file name.
	 * @param newFileName the new file name.
	 * @return true if the renaming succeeded / false otherwise.
	 * @throws IOException if an I/O error occured.
	 */
	virtual bool renameFile(char* oldFileName, char* newFileName)
 		throw(IOException) = 0;

	/**
	 * Returns the size of the file specified by <code>fileName</code>.
	 * @param fileName the name of the file whose size has to be returned.
	 * @return the size of the file specified by <code>fileName</code>.
	 * @throws IOException if an I/O error occured.
	 */
	virtual int getFileSize(char* fileName)
		throw(IOException) = 0;

	/**
	 * Returns a DirectoryListing object containing data about all the
	 * files in the <code>directoryName</code> directory.
	 * @param directoryName the name of the directory whose contents
	 *	has to be returned.
	 * @return an object which encapsulates all essesntial data about
	 *	the contents of the directory (see DirectoryListing class
	 *	for details).
	 * @throws IOException if an I/O error occured.
    	 */
	virtual DirectoryListing* listFiles(char* directoryName)
		throw(IOException) = 0;

	/**
	 * Formats the hard disk drive. This function:
	 * <ul>
	 * <li>clears all information currently stored on the hard disk;</li>
	 * <li>formats the special hard disk structurs in order to support the filesystem;</li>
	 * <li>anything else you want it to... :-)</li>
	 * </ul>
	 * @return true if formatting completed successfully / false otherwise.
	 * @throws IOException if an I/O error occured.
	 */
	virtual bool formatHDD()
 		throw(IOException) = 0;
};

#endif
