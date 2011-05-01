
#include "BinaryReader.h"

BinaryReader::BinaryReader()
{
	dwordBuffer = new dword_byte;

	wordBuffer = new word_byte;

	rgbBuffer = new rgb_byte;

	source = NULL;

	reset();
}

BinaryReader::BinaryReader(ByteBuffer* source)
{
	this->source = source;

	dwordBuffer = new dword_byte;

	wordBuffer = new word_byte;

	rgbBuffer = new rgb_byte;

	reset();
}

BinaryReader::~BinaryReader()
{
	delete dwordBuffer;
	delete wordBuffer;
	delete rgbBuffer;
}

byte BinaryReader::readByte(unsigned long from)
{
	ASSERT((from+sizeof(byte)) <= source->getLength());

	return source->getBytes()[from];
}

word BinaryReader::readWord(unsigned long from)
{
	ASSERT((from + WORD_SIZE) <= source->getLength());

	memcpy(wordBuffer->bytes, &source->getBytes()[from], WORD_SIZE);

	return wordBuffer->val;
}

dword BinaryReader::readRGB(unsigned long from)
{
	ASSERT((from + RGB_SIZE) <= source->getLength());

	memcpy(rgbBuffer->bytes, &source->getBytes()[from], RGB_SIZE);

	return rgbBuffer->val;
}

dword BinaryReader::readDword(unsigned long from)
{
	ASSERT((from + DWORD_SIZE) <= source->getLength());

	memcpy(dwordBuffer->bytes, &source->getBytes()[from], DWORD_SIZE);

	return dwordBuffer->val;
}

bool BinaryReader::seek(unsigned long seekTo)
{
	if (seekTo >= source->getLength())
		return false;

	seeker = seekTo;
	return true;
}

bool BinaryReader::skip(unsigned long count)
{
	if ((seeker + count) >= source->getLength())
		return false;

	seeker += count;
	return true;
}

dword BinaryReader::readDword()
{
	dword val = readDword(seeker);
	seeker += DWORD_SIZE;
	return val;
}

word BinaryReader::readWord()
{
	word val = readWord(seeker);
	seeker += WORD_SIZE;
	return val;
}

dword BinaryReader::readRGB()
{
	dword val = readRGB(seeker);
	seeker += RGB_SIZE;
	return val;
}

byte BinaryReader::readByte()
{
	return readByte(seeker++);
}

