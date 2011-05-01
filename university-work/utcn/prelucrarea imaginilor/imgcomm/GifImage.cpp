
#include "GifImage.h"

GifImage::GifImage()
{
	globalColorTable = NULL;
	localColorTable = NULL;
	compressedImageData = NULL;
	reader = NULL;
	controlExtension = NULL;
}

GifImage::GifImage(ByteBuffer* image)
{
	this->image = image;
	globalColorTable = NULL;
	localColorTable = NULL;
	compressedImageData = NULL;
	reader = NULL;
	controlExtension = NULL;
}

GifImage::~GifImage()
{
	if (globalColorTable != NULL)
		delete [] globalColorTable;
	if (localColorTable != NULL)
		delete [] localColorTable;
	if (compressedImageData != NULL)
		delete compressedImageData;
	if (reader != NULL)
		delete reader;
	if (controlExtension != NULL)
		delete controlExtension;
}

bool GifImage::init(ByteBuffer* image)
{
	this->image = image;
	return this->init();
}

bool GifImage::init()
{
	reader = new BinaryReader(this->image);
	reader->reset();

	if ((reader->readRGB() & 0x00FFFFFF) != GIF_SIGNATURE)
	{
		version = notGif;
		return false;
	}

	// read the version
	version = reader->readRGB();
	version &= 0x00FFFFFF;

	switch (version)
	{
	case gif87a:
		break;
	case gif89a:
		break;
	default:
		{
			version = notGif;
			return false;
		}
	}

	/* read the logical screen descriptor data */
	// read screen width
	screenWidth = reader->readWord();
	// read screen height
	screenHeight = reader->readWord();
	
	// read the packed fields
	byte temp;
	temp = reader->readByte();

	// the global color table
	if ((temp & 0x80) >> 7)
		globalColorTableFlag = true;
	else
		globalColorTableFlag = false;
	
	// the color resolution
	colorResolution = (temp & 0x70) >> 4;
	colorResolution++; // must be +1

	// the global sort flag
	if ((temp & 0x08) >> 3)
		globalSortFlag = true;
	else
		globalSortFlag = false;

	// the size of the global color table
	globalColorTableSize = 1 << ((temp & 0x07) + 1); // 2^[value read + 1]

	// background color index
	backgroundColorIndex = reader->readByte();
	// pixel aspect ratio
	pixelAspectRatio = reader->readByte();

	// read the global color table if any...
	if (globalColorTableFlag)
	{
		// alocate enough memory - allways the table is rgb based
		globalColorTable = new gif_rgb[globalColorTableSize];
		
		for (int i = 0; i < globalColorTableSize; i++)
		{
			globalColorTable[i].red = reader->readByte();
			globalColorTable[i].green = reader->readByte();
			globalColorTable[i].blue = reader->readByte();
		}
	}

	/* image descriptor */
	
	bool imageRead = false;
	while (reader->hasMoreElements())
	{

		imageSeparator = reader->readByte();
		
		// read the image
		if (imageSeparator == ImageSeparator)
		{
			imageRead = false;
			readImageDescriptor();
		}
		else if (imageSeparator == ExtensionIntroducer) // read an extension
		{
			byte label = reader->readByte();
			
			switch (label)
			{
			case GraphicControlLabel:
				{
					readGraphicControlExtension();
					break;
				}
			case CommentLabel:
				{
					readCommentExtension();
					break;
				}
			case PlainTextLabel:
				{
					readPlainTextExtension();
					break;
				}
			case ExtensionLabel:
				{
					readApplicationExtension();
					break;
				}
			default:
				{
					version = notGif;
					return false;
				}
			}
		}
		else
		{
			if (imageSeparator == GifTrailer && (!reader->hasMoreElements()))
			{
				return true;
			}
			else
			{
				break;
			}
		}
	}

	return false;
}

void GifImage::readImageDescriptor()
{
	// read image left position
	imageLeftPosition = reader->readWord();
	imageTopPosition = reader->readWord();
	imageWidth = reader->readWord();
	imageHeight = reader->readWord();

	// packed fields
	byte block;
	block = reader->readByte();

	localColorTableFlag = ((block & 0x80) >> 7) ? true : false;
	interlaceFlag = ((block & 0x40) >> 6) ? true : false;
	localSortFlag = ((block & 0x20) >> 5) ? true : false;

	localColorTableSize = 1 << ((block & 0x04) + 1); // 2^[value read + 1]

	// follows the local color table
	if (localColorTableFlag)
	{
		localColorTable = new gif_rgb[localColorTableSize];

		for (int i = 0; i < localColorTableSize; i++)
		{
			localColorTable[i].red = reader->readByte();
			localColorTable[i].green = reader->readByte();
			localColorTable[i].blue = reader->readByte();
		}
	}

	// follows the image data
	lzwMinimumCodeSize = reader->readByte();

	// and read the image data
	readImageData();
}

void GifImage::readImageData()
{
	bool readAllBlocks = false;

	compressedImageData = new ByteBuffer();

	while (!readAllBlocks)
	{
		byte blockSize = reader->readByte();
		
		if (blockSize == BlockTerminator)
		{
			readAllBlocks = true;
			continue;
		}

		for (int i = 0; i < blockSize; i++)
		{
			compressedImageData->addByte(reader->readByte());
		}
	}
}

void GifImage::readGraphicControlExtension()
{
	// read the block size
	byte block = reader->readByte();
	
	ASSERT(block == GraphicControlBlockSize); // it has a fixed value
	
	if (controlExtension != NULL)
		delete controlExtension;
	controlExtension = new ControlExtension();

	// handle the packed fields
	block = reader->readByte();

	controlExtension->disposalMethod = (block & 0x1C) >> 2;
	controlExtension->userInputFlag = ((block & 0x02) >> 1) ? true : false;
	controlExtension->transparentColorFlag = (block & 0x01) ? true : false;

	// read the delay time
	controlExtension->delayTime = reader->readWord();
	controlExtension->transparentColorIndex = reader->readByte();

	// must have a block terminator
	ASSERT(reader->readByte() == BlockTerminator);
}

void GifImage::readCommentExtension()
{
	// supposed to skip over the coments
	// just read the comment data
	byte blockSize;

	bool readAllBlocks = false;

	// read the comment blocks
	while (!readAllBlocks)
	{
		blockSize = reader->readByte();
		
		// if the block size is a block terminator
		if (blockSize == BlockTerminator)
		{
			readAllBlocks = true;
			continue;
		}

		// do not perform any read procedure
		reader->skip(blockSize);
	}
}

void GifImage::readPlainTextExtension()
{
	// supposed to skip over plain text
	byte blockSize = reader->readByte();
	
	ASSERT(blockSize == PlainTextBlockSize); // must have a value of 12

	// skip the data header
	reader->skip(blockSize);

	// read plain text data
	bool readAllBlocks = false;

	while (!readAllBlocks)
	{
		blockSize = reader->readByte();

		if (blockSize == BlockTerminator)
		{
			readAllBlocks = true;
			continue;
		}

		reader->skip(blockSize);
	}
}

void GifImage::readApplicationExtension()
{
	// supposed to skip over aplication extension
	byte blockSize = reader->readByte();

	ASSERT(blockSize == AplicationBlockSize);

	reader->skip(blockSize);

	// read application data
	bool readAllBlocks = false;

	while (!readAllBlocks)
	{
		blockSize = reader->readByte();

		if (blockSize == BlockTerminator)
		{
			readAllBlocks = true;
			continue;
		}

		reader->skip(blockSize);
	}
}


ByteBuffer* GifImage::decompressImageData()
{
	ByteBuffer* rawData = new ByteBuffer(imageWidth*imageHeight);

	LZWCompressor* compressor = new LZWCompressor(rawData, compressedImageData);

	compressor->setStartNoOfBits(lzwMinimumCodeSize);

	compressor->expand();

	return compressor->getRawData();
}

/**
 * Debug method.
 */
void GifImage::writeCompressedDataToFile(char* filename)
{
	FILE* f = fopen(filename, "wb");
	if (f == NULL)
	{
		printf("Unable to open file %s\n", filename);
		exit(1);
	}

	compressedImageData->resetSeeker();
	for (unsigned long i = 0; i < compressedImageData->getLength(); i++)
	{
		putc(compressedImageData->getByte(), f);
	}

	fclose(f);
}

