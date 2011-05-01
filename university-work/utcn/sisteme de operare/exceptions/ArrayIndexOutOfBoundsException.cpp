// Project: os file system
// Creation date: Oct 27, 2002
// Author: bogdan
// File name: ArrayIndexOutOfBoundsException.cpp
// Description: ...

#include "../include/exceptions/ArrayIndexOutOfBoundsException.h"

ArrayIndexOutOfBoundsException::ArrayIndexOutOfBoundsException(int idx)
{
	index = idx;
}

char* ArrayIndexOutOfBoundsException::getMessage()
{
	strstream message;
	message << "Index " << index << " was out of bounds." << ends;

	return message.str();
}
