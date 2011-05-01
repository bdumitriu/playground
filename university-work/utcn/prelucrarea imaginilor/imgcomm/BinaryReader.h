
#ifndef _BINARY_READER_H_
#define _BINARY_READER_H_

#include "ByteBuffer.h"
#include "debug.h"


/**
 * The class is used to read the same 3, 4, 2 bytes regardless of the
 * little-big endian representation. It is used as a wrapper around a
 * <code>ByteBuffer</code> object, which cannot perform the read operation
 * upon itself. <br /><br />
 *
 * Author	Tudor Marian <br />
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * @author Tudor Marian
 */
class BinaryReader
{
public:
	/**
	* Creates a reader object.
	*/
	BinaryReader();

	/**
	* Creates a reader object and binds it to the reading source.
	* 
	* @param	source	the <code>ByteBufer</code> used to be read.
	*/
	BinaryReader(ByteBuffer* source);
	
	/**
	* Destroys the object.
	*/
	~BinaryReader();

	/**
	* Reads a 4 byte chunk and converts it into a double word integer value.
	*
	* @param	from	the address the 4 bytes are to be read.
	* @return	the converted data.
	*/
	dword readDword(unsigned long from);

	/**
	* Reads a 2 byte chunk and converts it into a word integer value.
	*
	* @param	from	the address the 2 bytes are to be read.
	* @return	the converted data.
	*/
	word readWord(unsigned long from);


	/**
	* Reads a 3 byte chunk and converts it into a double word integer value.
	*
	* @param	from	the address the 3 bytes are to be read.
	* @return	the converted data.
	*/
	dword readRGB(unsigned long from);
	
	/**
	* Reads a byte.
	*
	* @param	from	the address the byte is to be read.
	* @return	the plain byte.
	*/
	byte readByte(unsigned long from);

	/**
	* Returns the ByteBuffer data source object.
	*
	* @return	the reading source object.
	*/
	ByteBuffer* getSource() {return this->source;};

	/**
	* Sets the ByteBuffer data source object to be read.
	*
	* @param	source	the reader source.
	*/
	void setSource(ByteBuffer* source) {this->source = source;};

	/**
	* The metod resets the seek element to the origin, meaning that
	* any further unparametrized read methods will start at the first
	* byte in the reading source.
	*/
	void reset() {seeker = 0;};

	/**
	* The method puts the seeker element to the speciffied index.
	*
	* @param	seekTo	the location where the <code>seeker</code> element
	*	will point from now on.
	* @return	false if we want to seek beyond the size of the source, true
	*	otherwise.
	*/
	bool seek(unsigned long seekTo);
	
	/**
	* The method makes the seeker element to skip <code>count</code> bytes.
	* 
	* @param	count	the number of bytes to skip forward.
	* @return	false if the skip occurs beyond the bounds of the source's
	*	<code>length</code>, true otherwise.
	*/
	bool skip(unsigned long count);

	/**
	* Reads a 4 byte double word from the source starting at the address given
	* by the <code>seeker</code> attribute.
	*
	* @return	the decoded value of the 4 bytes as a little endian double word.
	*/
	dword readDword();

	/**
	* Reads a 2 byte word from the source starting at the address given
	* by the <code>seeker</code> attribute.
	*
	* @return	the decoded value of the 2 bytes as a little endian word.
	*/
	word readWord();

	/**
	* Reads a 3 byte double word from the source starting at the address given
	* by the <code>seeker</code> attribute.
	*
	* @return	the decoded value of the 3 bytes as a little endian double word.
	*/
	dword readRGB();

	/**
	* Reads a byte from the source starting at the address given
	* by the <code>seeker</code> attribute.
	*
	* @return	the byte read.
	*/
	byte readByte();

	/**
	* Checks to see if there are any more elements to read, considering the
	* position given by the <code>seeker</code> within the source object.
	*
	* @return	true if there are any more readable bytes, false otherwise.
	*/
	bool hasMoreElements() {return (seeker >= source->getLength()) ? false : true;};

private:
	/** The source object - from where the reading is to be done.
	*/
	ByteBuffer* source;

	/** The position where the reading process will take place from the source.
	*/
	unsigned long seeker;
	
	/** A structure used to convert 4 bytes into a double word.
	*/
	dword_byte* dwordBuffer;

	/** A structure used to convert 2 bytes into a word.
	*/
	word_byte* wordBuffer;

	/** A structure used to convert 3 bytes into a double word.
	*/
	rgb_byte* rgbBuffer;
};

#endif //_BINARY_READER_H_

