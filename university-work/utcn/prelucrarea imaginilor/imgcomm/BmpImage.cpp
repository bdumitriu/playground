
#include "BmpImage.h"

// return the bytes of the row including padding bytes
long BmpImage::rowBytes()
{
	int numBits = planes*bitsPerPixel*width;

	return ((numBits + 31)/32)*4;
}

BmpImage::BmpImage()
{
	colorPalette = NULL;
	//this->init();
}

BmpImage::BmpImage(ByteBuffer* image)
{
	colorPalette = NULL;
	this->image = image;
	//this->init();
}

BmpImage::~BmpImage()
{
	if ((colorPaletteSize != 0) && (colorPalette != NULL))
		delete [] colorPalette;
}

bool BmpImage::init(ByteBuffer* image)
{
	this->image = image;
	return this->init();
}

/**
 * Bring values to default.
 */
void BmpImage::initValuesToDefault()
{
	version = NOTBMP;
	bmpBits = NULL;
	colorPaletteSize = 0;
	
	fileType = NOT_BMP;
	fileSize = 0;
	Xhot = 0;
	Yhot = 0;
	bitmapOffset = 0;
	headerSize = 0;
	width = 0;
	height = 0;
	planes = 0;
	bitsPerPixel = 0;
	compression = 0;
	bitmapSize = 0;
	horizResolution = 0;
	vertResolution = 0;
	colorsUsed = 0;
	colorsImportant = 0;
	units = 0;
	reserved = 0;
	recording = 0;
	rendering = 0;
	size1 = 0;
	size2 = 0;
	identifier = 0;
}

/**
 *	Just read the bitmap file.
 *
 */

bool BmpImage::init()
{
	initValuesToDefault();

	BinaryReader* reader = new BinaryReader(this->image);
	reader->reset();

	/* read the file data header */
	// read the type of the file
	reader->seek(FILE_TYPE_OFFSET);
	fileType = reader->readWord();
	

	switch (fileType)
	{
	case BMPBITMAP:
		break;
	case BMPARRAY:
	case BMPCLRICON:
	case BMPCLRPOINTER:
	case BMPICON:
	case BMPPOINTER:
		return false;
	default:
		{
			fileType = NOT_BMP; // exception should be thrown
			return false;
		}
	}

	// read the file size in bytes
	fileSize = reader->readDword();

	// read the two hot spots, reserved 1 & reserved 2
	Xhot = reader->readWord();
	Yhot = reader->readWord();

	bitmapOffset = reader->readDword();

	/* read the bitmap header now */
	headerSize = reader->readDword();

	if (headerSize < BMPOLDANYHDRSIZE || 
		(headerSize > BMPNEWOS2HDRSIZE && headerSize != BMPNEWWINV4HDRSIZE))
	{
		fileType = NOT_BMP; // exception should be thrown
		return false;
	}

	// read the rest of the header and the palette if any
	if (headerSize == BMPOLDANYHDRSIZE) // we have an old BMP file format
	{
		version = BMPOLD;
		width = (word) reader->readWord();
		height = (word) reader->readWord();
		planes = reader->readWord();
		bitsPerPixel = reader->readWord();

		if (bitsPerPixel != 1 || bitsPerPixel != 4 || bitsPerPixel != 8 || 
			bitsPerPixel != 24)
		{
			fileType = NOT_BMP; // exception should be thrown
			return false;
		}

		colorPaletteSize = bitsPerPixel == 24 ? 0 : 1 << bitsPerPixel;
		
		int numberOfEntries = (bitmapOffset - BMPFILEHDRSIZE - 
			BMPOLDANYHDRSIZE) / BMPOLDPALETTESIZE;

		if (colorPaletteSize != numberOfEntries)
		{
			fileType = NOT_BMP; // exception should be thrown
			return false;
		}

		// read the palette
		if (colorPaletteSize != 0)
		{
			colorPalette = new unsigned long[colorPaletteSize];
			for (int i = 0; i < colorPaletteSize; i++)
			{
				colorPalette[i] = reader->readRGB();
			}
		}

		bitmapSize = rowBytes() * height;
	}
	else // new BMP file format
	{
		switch (headerSize)
		{
		case BMPNEWWINHDRSIZE:
			{
				version = BMPWINNEW;
				break;
			}
		case BMPNEWOS2HDRSIZE:
			{
				version = BMPOS2NEW;
				break;
			}
		case BMPNEWWINV4HDRSIZE:
			{
				version = BMPWIN4NEW;
				break;
			}
		default:
			{
				version = BMPUNKNOWN;
				break;
			}
		}
		width = reader->readDword();
		height = reader->readDword();
		planes = reader->readWord();
		bitsPerPixel = reader->readWord();
		compression = reader->readDword();
		bitmapSize = reader->readDword();
		horizResolution = (long) reader->readDword();
		vertResolution = (long) reader->readDword();
		colorsUsed = reader->readDword();
		colorsImportant = reader->readDword();

		bitmapSize = (bitmapSize) ? bitmapSize : rowBytes() * height;
		
		
		// if the image is an OS2 bitmap
		if (headerSize == BMPNEWOS2HDRSIZE)
		{
			units = reader->readWord();
			reserved = reader->readWord();
			recording = reader->readWord();
			rendering = reader->readWord();
			size1 = reader->readDword();
			size2 = reader->readDword();
			identifier = reader->readDword();
		}

		// read the palette
		// seek to the color palette
		reader->seek(BMPFILEHDRSIZE + headerSize);
		// rle compression not supported
		if (compression == 3 && (bitsPerPixel == 16 || bitsPerPixel == 32))
		{
			// should throw exception
			return false;
		}
		// compute the number of color table entries
		colorPaletteSize = bitsPerPixel > 8 ? 0 : 1 << bitsPerPixel;
		
		int numberOfEntries = (bitmapOffset - BMPFILEHDRSIZE - 
			headerSize) / BMPNEWPALETTESIZE;

		if (colorPaletteSize != numberOfEntries)
		{
			fileType = NOT_BMP; // exception should be thrown
			return false;
		}
			
		// read the palette
		if (colorPaletteSize != 0)
		{
			colorPalette = new unsigned long[colorPaletteSize];
			for (int i = 0; i < colorPaletteSize; i++)
			{
				colorPalette[i] = reader->readDword();
			}
		}
	}

	// shold read the data bits & lose the padding
	bmpBits = &image->getBytes()[bitmapOffset];

	// calculate the file size if not given
	if (fileSize == 0)
	{
		fileSize = BMPFILEHDRSIZE;
		fileSize += headerSize;

		int paletteSize = (version == BMPOLD) ? BMPOLDPALETTESIZE : BMPNEWPALETTESIZE;

		fileSize += colorPaletteSize*paletteSize;
		fileSize += bitmapSize;
	}

	delete reader;
	return true;
}

/**
 * BmpImage::serialize
 *	The method will create a byte stream out of the BMP image object, 
 *	stream that can be later written to file as raw bitmap image.
 */
ByteBuffer* BmpImage::serialize()
{
	if (fileType == 0)
		return NULL;
	ByteBuffer* img = new ByteBuffer(this->fileSize);
	
	// write the file header
	*img << fileType;
	*img << fileSize; 
	*img << Xhot;
	*img << Yhot;
	*img << bitmapOffset;

	// the bmp file header
	if (version == NOTBMP || version == BMPUNKNOWN)
		return NULL;

	// add the size of the bmp header
	*img << headerSize;

	if (version == BMPOLD)
	{
		// because of the unified treatment
		img->addWord((word) width);
		img->addWord((word) height);

		*img << (word) planes;
		*img << (word) bitsPerPixel;

		// add the palette
		for (int i = 0; i < colorPaletteSize; i++)
		{
			// suppose to add 3 byte rgb structures
			img->addRGB(colorPalette[i]);
		}

	}
	else
	{
		*img << width;
		*img << height;
		*img << planes;
		*img << bitsPerPixel;
		*img << compression;
		*img << bitmapSize;
		*img << (dword) horizResolution;
		*img << (dword) vertResolution;
		*img << colorsUsed;
		*img << colorsImportant;
		
		if (version == BMPOS2NEW) // OS2 header
		{
			// add up the rest of the data until 64 bytes
			*img << units;
			*img << reserved;
			*img << recording;
			*img << rendering;
			*img << size1;
			*img << size2;
			*img << identifier;
		}
		
		if (version == BMPNEWWINV4HDRSIZE)
		{
			// seek backwards and init to 0 everything
			img->seek(BMPFILEHDRSIZE + BMPNEWWINHDRSIZE);
			
			for (int i = BMPFILEHDRSIZE + BMPNEWWINHDRSIZE; i < BMPNEWWINV4HDRSIZE; i++)
			{
				*img << (byte) 0x00;
			}
		}

		// skip to the color table
		printf("\nposition : %d\n", img->getPosition());
		printf("length: %d\n", img->getLength());
		printf("seek to : %d\n", BMPFILEHDRSIZE + headerSize);
		img->seek(BMPFILEHDRSIZE + headerSize);

		for (int i = 0; i < colorPaletteSize; i++)
		{
			*img << colorPalette[i];
		}
	}

	img->seek(bitmapOffset);

	for (unsigned int i = 0; i < bitmapSize; i++)
	{
		*img << bmpBits[i];
	}

	//img->trimToSize();
	return img;
}

void BmpImage::toGrayscale()
{
	if (bitsPerPixel != 8 && bitsPerPixel != 24)
		return;

	int w = rowBytes();
	int avg;

	if (bitsPerPixel == 24)
	{
		ByteBuffer* bmpBuffer = new ByteBuffer(bitmapSize, bmpBits);
		BinaryReader* reader = new BinaryReader(bmpBuffer);

		byte blue, green, red;

		for (unsigned int i = 0; i < height; i++)
		{
			for (unsigned int j = 0; j < width; j++)
			{
				blue = reader->readByte(i*w+j*3);
				green = reader->readByte(i*w+j*3+1);
				red = reader->readByte(i*w+j*3+2);

				avg = (red + green + blue)/3;
				avg = (avg << 16) | (avg << 8) | (avg);

				bmpBuffer->seek(i*w+j*3);
				bmpBuffer->addRGB(avg);
			}
		}

		bmpBits = bmpBuffer->getBytes();
		return;
	}

	int colorTableIndex;

	for (unsigned int i = 0; i < height; i++)
	{
		for (unsigned int j = 0; j < width; j++)
		{
			colorTableIndex = bmpBits[i*w+j];
			avg = (colorPalette[colorTableIndex] & 0x00FF0000) >> 16;
			avg += (colorPalette[colorTableIndex] & 0x0000FF00) >> 8;
			avg += (colorPalette[colorTableIndex] & 0x000000FF);
			avg /= 3;

			bmpBits[i*w+j] = avg;
		}
	}

	for (int k = 0; k < colorPaletteSize; k++)
	{
		colorPalette[k] = (k << 16) | (k << 8) | (k);
	}
}

