/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 11 2002
 * Description:		This is the interface of the PathParser class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _PATH_PARSER_H
#define _PATH_PARSER_H

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include "../defs.h"

/**
 * This class allows you to parse a path of the form /usr/local/bin/filename
 * or /usr/local/bin/ and retrive its elements one by one. That means the
 * first time you call next() you get usr, then you get local and so on. The
 * default path separator is include/defs.h::PATH_SEPARATOR, but you have the option of changing it.
 *
 * @short A class to break paths into elements.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class PathParser
{
public:
	/**
	 * Creates a new PathParser for the given <code>path</code>.
	 *
	 * @param path the path you want to parse.
	 */
	PathParser(const char* path);

	/**
	 * Creates a new PathParser for the given <code>path</code> and
	 * sets the path separator to <code>pathSeparator</code>.
	 *
	 * @param path the path you want to parse.
	 * @param pathSeparator the path separator to use.
	 */
	PathParser(const char* path, const char pathSeparator);

	/**
	 * The destructor of the class.
	 */
	~PathParser();

	/**
	 * Returns true if there are more path elements to read
	 * and false if the path has completely traversed.
	 *
	 * @return true if there are more path elements to read
	 * and false if the path has completely traversed.
	 */
	bool hasMore();

	/**
	 * Returns the next path element. If no more elements are
	 * available (i.e., hasMore() returns false), NULL is returned.
	 *
	 * @return the next path element.
	 */
	char* next();

	/**
	 * Returns true if the first character of the path is
	 * the path separator and false otherwise.
	 *
	 * @return true if the first character of the path is
	 * the path separator and false otherwise.
	 */
	bool startsWithPathSeparator();

	/**
	 * Returns true if the last character of the path is
	 * the path separator and false otherwise.
	 *
	 * @return true if the last character of the path is
	 * the path separator and false otherwise.
	 */
	bool endsWithPathSeparator();

	/**
	 * Returns the value of the path data member.
	 *
	 * @return the value of the path data member.
	 */
	char* getPath();

	/**
	 * Returns the value of the pathSeparator data member.
	 *
	 * @return the value of the pathSeparator data member.
	 */
	char getPathSeparator();

private:
	unsigned long pos;		/* the position in the string where we got */
	char* path;			/* the path */
	char pathSeparator;		/* the path separator */
	unsigned long pathLength;	/* the output of strlen(path) */
};

#endif
