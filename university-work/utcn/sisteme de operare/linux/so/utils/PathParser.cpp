/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 11 2002
 * Description:		This is the implementation of the PathParser class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/utils/PathParser.h"

PathParser::PathParser(const char* path)
{
	this->path = new char[strlen(path)+1];
	if (this->path == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->path, path);
	this->path[strlen(path)] = '\0';
	pathSeparator = PATH_SEPARATOR;
	pos = 0;
	pathLength = strlen(this->path);
}

PathParser::PathParser(const char* path, const char pathSeparator)
{
	this->path = new char[strlen(path)+1];
	if (this->path == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->path, path);
	this->path[strlen(path)] = '\0';
	this->pathSeparator = pathSeparator;
	pos = 0;
	pathLength = strlen(this->path);
}

PathParser::~PathParser()
{
	if (path != NULL)
		delete [] path;
}

bool PathParser::hasMore()
{
	while ((pos < pathLength) && (path[pos] == pathSeparator))
		pos++;

	if (pos == pathLength)
		return false;
	else
		return true;
}

char* PathParser::next()
{
	while ((pos < pathLength) && (path[pos] == pathSeparator))
		pos++;

	if (pos == pathLength)
		return NULL;

	// find the next occurence of the path separator in path
	unsigned long nextSepPos = pos;
	while ((nextSepPos < pathLength) && (path[nextSepPos] != pathSeparator))
		nextSepPos++;

	char* retVal = new char[nextSepPos-pos+1];
	if (retVal == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// copy the path element in the return value
	memcpy(retVal, path+pos, nextSepPos-pos);

	retVal[nextSepPos-pos] = '\0';

	// update pos
	pos = nextSepPos;

	return retVal;
}

bool PathParser::startsWithPathSeparator()
{
	if (pathLength > 0)
		if (path[0] == pathSeparator)
			return true;

	return false;
}

bool PathParser::endsWithPathSeparator()
{
	if (pathLength > 0)
		if (path[strlen(path)-1] == pathSeparator)
			return true;

	return false;
}

char* PathParser::getPath()
{
	return path;
}

char PathParser::getPathSeparator()
{
	return pathSeparator;
}
