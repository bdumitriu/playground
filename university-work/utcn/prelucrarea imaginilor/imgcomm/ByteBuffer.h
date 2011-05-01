

#ifndef _BYTEBUFFER_H_
#define _BYTEBUFFER_H_

#include "debug.h"

/** The default size of the buffer.
*/
#define DEFAULT_BUFFER_SIZE 1024

/**
 *	This is a class usefull for storing an array of bytes. It replaces
 *	the common strstream which is limited to char type. I should have
 *	made it work with templates but when I realized that I was nearly done.
 *
 *	WARNING: no syncronizations implemented on the list due to the
 *	loss of portability it would have brought. <br /><br />
 *
 * Author	Tudor Marian <br />
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * @author  Tudor Marian
 */
class ByteBuffer
{
public:
	/**
	* Creates a ByteByffer object.
	*/
	ByteBuffer();

	/**
	* Creates a ByteBuffer of size <code>size</code>.
	* @param	size	the size of the initial buffer.
	*/
	ByteBuffer(unsigned long size);

	/** 
	* Creates a ByteBuffer of size <code>size</code>, and
	* content <code>data</code>. The data is passed by
	* reference, because operations such as trim or increase
	* in size might occur thus modifying the start address of
	* the data buffer.
	*
	* @param	size	the size of the data.
	* @param	data	the array of bytes holding the data.
	*/
	ByteBuffer(unsigned long size, byte* data);

	 /**
	 * Destroys the data buffer.
	 */
	~ByteBuffer();

	/**
	* Moves the position pointer at the location given by the 
	* <code>seekAt</code> parameter, or <code>length</code> if
	* the parameter goes beyond the <code>length</code>.
	*
	* @param	seekAt	position where the seeker will point to.
	*/
	void seek(unsigned long seekAt);

	/**
	* Advances the position pointer forward with <code>count</code>
	* positions.
	*
	* @param	count	the number of bytes to skip.
	*/
	void skip(unsigned long count);

	/**
	* Trims the buffer to the exact size of the byte data contained.
	*/
	void trimToSize();

	/**
	* Returns the size of the buffer.
	*
	* @return	the size of the buffer.
	*/
	unsigned long getSize() {return size;};

	/**
	* Returns the length of the buffer.
	*
	* @return	the length of the buffer.
	*/
	unsigned long getLength() {return length;};

	/**
	* Reset the seeker to the zero position.
	*/
	void resetSeeker() {position = 0;};

	/**
	* Returns the byte buffer data.
	*
	* @return	the byte array data.
	*/
	byte* getBytes() {return buffer;};

	/**
	* Adds a new byte at the position given by the <code>seeker</code>
	* attribute.
	*
	* @param	b	the byte to be added.
	*/
	void addByte(byte b);

	/**
	* Adds a new word in little endian format into the byte array.
	*
	* @param	w	the 2 byte word to be added.
	*/
	void addWord(word w);

	/**
	* Adds a double word in little endian format into the byte array.
	*
	* @param	d	the 4 byte double word to be added starting at <code>seeker</code>.
	*/
	void addDword(dword d);

	/**
	* Adds 3 bytes in little endian format into the byte array.
	*
	* @param	rgb	the red-green-blue 3 bytes packed into a 4 byte double word
	*	to be written to the byte array.
	*/
	void addRGB(dword rgb);
	
	/**
	* Overrides the << operator and does the same thing as the <code>addByte</code>
	* method.
	*/
	friend void operator << (ByteBuffer& buffer, byte b);

	/**
	* Overrides the << operator and does the same thing as the <code>addDword</code>
	* method.
	*/
	friend void operator << (ByteBuffer& buffer, dword d);

	/**
	* Overrides the << operator and does the same thing as the <code>addWord</code>
	* method.
	*/
	friend void operator << (ByteBuffer& buffer, word w);
	
	/**
	* Checks to see if the buffer is trimed (it optimally occupies it's buffer).
	*
	* @return	true if the buffer is trimed, false otherwise.
	*/
	bool isTrimed() { return (length == size);};

	/**
	* Returns the byte found at <code>position</code> index within the byte
	* array.
	*
	* @return	the byte found or -1 if the end of the array has been reached.
	*/
	int getByte();

	// clears the buffer content - policy will deallocate the buffer only
	// if the size is larger than DEFAULT_BUFFER_SIZE
	/**
	* Clears the buffer content, according to the policy the buffer will be
	* deallocated if the size is larger than DEFAULT_BUFFER_SIZE, and reallocated
	* as a data chunck of DEFAULT_BUFFER_SIZE size.
	*/
	void clear();

	// return the pointer position
	/**
	* Return the position within the array.
	*
	* @return	the value of the position attribute.
	*/
	unsigned long getPosition() {return position;};

private:
	
	/** The byte buffer.
	*/
	byte* buffer;
	
	/** The buffer's full size.
	*/
	unsigned long size;

	/** The buffer's length - only valid data.
	*/
	unsigned long length;

	/** The position of the marker.
	*/
	unsigned long position;

	/** 
	* Recalculates the size && makes the memcopy procedure. The size is
	* increased with 1/2 the current length and the data is copied into
	* the new buffer.
	*/
	void recalculateSize();

	/** Buffers needed to write back more than one byte data.
	*/
	dword_byte dwordBuffer;
	word_byte wordBuffer;
	rgb_byte rgbBuffer;
};


#endif //_BYTEBUFFER_H_


