
/**
 * @Author Tudor Marian
 *
 * Technical University Of Cluj-Napoca
 * Computer Science Departament
 * gr. 3241/1
 *
 * created: april, 13, 2003
 * last modified: april 19, 2003
 *
 */

#include "ByteBuffer.h"

ByteBuffer::ByteBuffer()
{
	size = DEFAULT_BUFFER_SIZE;
	buffer = new byte[this->size];
	resetSeeker();
	length = 0;
}

ByteBuffer::ByteBuffer(unsigned long size)
{
	this->size = size;
	buffer = new byte[this->size];
	resetSeeker();
	length = 0;
}

/**
 * because the buffer is given, and because increase/trim operations might 
 * occur the buffer is given by reference (a pointer to the array start 
 * address).
 */
ByteBuffer::ByteBuffer(unsigned long size, byte* data)
{
	this->size = size;
	buffer = new byte[this->size];

	memcpy(buffer, data, this->size);

	//resetSeeker();
	this->position = this->size;
	length = size;
	resetSeeker();
}

ByteBuffer::~ByteBuffer()
{
	delete [] buffer;
}

void ByteBuffer::seek(unsigned long seekAt)
{
	if (seekAt < 0)
	{
		position = 0;
		return;
	}
	if (seekAt >= size)
		position = size;
	else
		position = seekAt;

	if (length < position)
			length = position;
}

void ByteBuffer::skip(unsigned long count)
{
	if ((position += count) >= size)
		position = size - 1;

	if (length < position) 
		length = position;
}

void ByteBuffer::trimToSize()
{
	if (size == length)
		return;

	byte* temp = new byte[length];

	memcpy(temp, buffer, length * sizeof(byte));

	delete buffer;

	buffer = temp;

	size = length;
}

void ByteBuffer::addByte(byte b)
{
	ASSERT(position >= 0);
	
	if (position == size) // meaning it's the last byte space
	{
		recalculateSize();
	}

	// now go ahead
	buffer[position ++] = b;
	if (length < position)
		length = position;
}

void ByteBuffer::recalculateSize()
{
	int newSize = (int)((size * 3)/2 + 1);
	

	byte* temp = new byte[newSize];

	memcpy(temp, buffer, size * sizeof(byte));

	delete buffer;

	buffer = temp;

	size = newSize;
}

int ByteBuffer::getByte()
{
	if (position == length)
		return -1;

	return buffer[position++];
}

void ByteBuffer::clear()
{
	if (size < DEFAULT_BUFFER_SIZE)
	{
		length = 0;
		resetSeeker();
	}
	else
	{
		size = DEFAULT_BUFFER_SIZE;
		byte* temp = new byte[DEFAULT_BUFFER_SIZE];
		length = 0;
		resetSeeker();

		delete [] buffer;

		buffer = temp;
	}
}

void operator << (ByteBuffer& buffer, byte b)
{
	buffer.addByte(b);
}

void ByteBuffer::addDword(dword d)
{
	dwordBuffer.val = d;

	*this << dwordBuffer.bytes[0];
	*this << dwordBuffer.bytes[1];
	*this << dwordBuffer.bytes[2];
	*this << dwordBuffer.bytes[3];
}

void operator << (ByteBuffer& buffer, dword d)
{
	buffer.addDword(d);
}

void ByteBuffer::addWord(word w)
{
	wordBuffer.val = w;

	*this << wordBuffer.bytes[0];
	*this << wordBuffer.bytes[1];
}

void operator << (ByteBuffer& buffer, word w)
{
	buffer.addWord(w);
}


void ByteBuffer::addRGB(dword rgb)
{
	rgbBuffer.val = rgb;
	
	*this << rgbBuffer.bytes[0];
	*this << rgbBuffer.bytes[1];
	*this << rgbBuffer.bytes[2];
}



