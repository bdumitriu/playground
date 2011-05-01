
#ifndef _BMPIMAGE_H_
#define _BMPIMAGE_H_

#include "debug.h"
#include "ByteBuffer.h"
#include "BinaryReader.h"

/** The types of the bitmap file.
*/
enum BmpTypes
{
	BMPARRAY		= 0x4142,	// 'BA'
	BMPBITMAP		= 0x4D42,	// 'BM'
	BMPCLRICON		= 0x4943,	// 'CI'
	BMPCLRPOINTER	= 0x5043,	// 'CP'
	BMPICON			= 0x4349,	// 'IC'
	BMPPOINTER		= 0x5450,	// 'PT'
	NOT_BMP			= 0x0000,
};

/** The sizes allowed for the bitmap file header.
*/
enum BmpSizes
{
   BMPFILEHDRSIZE   = 14,
   BMPOLDANYHDRSIZE = 12,
   BMPNEWWINHDRSIZE = 40,
   BMPNEWOS2HDRSIZE = 64,
   BMPNEWWINV4HDRSIZE = 108,
};

/** The valid palette size in bytes per color.
*/
enum BmpPaletteSize
{
	BMPOLDPALETTESIZE	= 3,
	BMPNEWPALETTESIZE	= 4,
};

/** The versions of the bitmap file.
*/
enum BmpVersions
{
	NOTBMP,
	BMPOLD, 
	BMPWINNEW,
	BMPOS2NEW,
	BMPWIN4NEW,
	BMPUNKNOWN,
};

/** The file offset.
*/
#define FILE_TYPE_OFFSET 0

/**
* The class describes a file format exception.
*/
class FileFormatException
{
public:
	FileFormatException(char* message){this->message = message;};
	~FileFormatException(){};
	char* toString(){return this->message;};
private:
	char* message;
};

/**
 * This class is a wrapper around a bmp image. It is responsible
 * for the extraction process of all the characteristics of a bmp file. <br /><br />
 * 
 * Author Tudor Marian <br />
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * @Author Tudor Marian
 */
class BmpImage
{
public:
	/**
	* Creates a bitmap image object.
	*/
	BmpImage();

	/**
	* Creates a bitmap image object.
	*
	* @param	image	the serialized form of the BMP file.
	*/
	BmpImage(ByteBuffer* image);//throw (FileFormatException);
	~BmpImage();

	 /** 
	 * The method initializes all fields of the object with apropriate values
	 * loaded from the BMP file, file found serialized into the ByteBuffer
	 * object.
	 *
	 * @return true if the bitmap has been read successfully, false otherwise.
	 */
	bool init();// throw (FileFormatException);

	 /** 
	 * The method initializes all fields of the object with apropriate values
	 * loaded from the BMP file, file found serialized into the ByteBuffer
	 * object.
	 *
	 * @param	image	the serialized form of the BMP file.
	 * @return	true if the bitmap has been read successfully, false otherwise.
	 */
	bool init(ByteBuffer* image);// throw (FileFormatException);


	/* the following methods are used to retrieve the bmp contained data */

	/**
	* Returns the bitmap data, representing indexes into the color table if a
	* color table is available, or plain color triplet elsewhere.
	*
	* @return	the bitmap data.
	*/
	byte* getBitmap(){return this->bmpBits;};

	/**
	* Returns the size of the bitmap data in bytes.
	*
	* @return	the size of the bitmap array.
	*/
	long getBitmapSize() {return bitmapSize;};

	/**
	* The file type.
	*
	* @return	the bitmap file type.
	*/
	dword getFileType() {return this->fileType;};

	/**
	* Returns the width of the image in pixels.
	*
	* @return	the width of the image in pixels.
	*/
	long getWidth() {return (long)this->width;};
	
	/**
	* Returns the height of the image in pixels.
	*
	* @return	the height of the image in pixels.
	*/
	long getHeight() {return (long)this->height;};
	
	/**
	* Returns the depth of the image in number of bits per pixel.
	*
	* @return	the depth of the image.
	*/
	long depth() {return planes*bitsPerPixel;};

	/**
	* Returns the number of planes of the image.
	*
	* @return	the number of planes, usually this value is 1.
	*/
	int noOfPlanes() {return (int)planes;};
	
	/** 
	* The number of bits per pixel per plane.
	*
	* @return	the number of bits per pixel per plane.
	*/
	long noOfBitsPerPixel(){return bitsPerPixel;};
	
	/**
	* The compression type.
	*
	* @return	the compression type.
	*/
	long compressionType(){return (long)compression;};

	/**
	* Returns the horizontal resolution as pixel per meter.
	*
	* @return	the horizontal resolution.
	*/
	long xResolution() {return horizResolution;};
	
	/**
	* Returns the vertical resolution as pixel per meter.
	*
	* @return	the vertical resolution.
	*/
	long yResolution() {return vertResolution;};

	/**
	* Calculates the size of the scanline (must be aligned at 4 bytes bound).
	*
	* @return	the scanline size in bytes.
	*/
	long rowBytes();

	// returns the size of the bitmap
	long size() {return bitmapSize ? bitmapSize : rowBytes() * height;};

	/**
	* Returns the color palette if one exists, NULL otherwise.
	*
	* @return	the color pallete, or NULL if no color palette is available.
	*/
	dword* getColors() {return colorPaletteSize ? colorPalette : NULL;};

	/**
	* Returns the number of colors in the color palette.
	*
	* @param	the number of colors of the color table.
	*/
	int numberOfColors() {return colorPaletteSize;};

	/**
	* Serializes the bitmap image file.
	*
	* @return	the serialized form of the BMP file.
	*/
	ByteBuffer* serialize();

	/**
	* Converts the image into a grayscale image.
	*/
	void toGrayscale();

	/**
	* Converts a 24 bit bitmap to a 8 bit bitmap.
	*/
	bool convert24BitBitmapTo8Bit();

private:
	/** The image buffer.
	*/
	ByteBuffer* image;

	/** The version of the bitmap image.
	*/
	BmpVersions version;

	/** The bitmap data byte stream.
	*/
	byte* bmpBits;

	/** The color palette, if it exists.
	*/
	dword* colorPalette; // maximum size is 256

	/** The size of the color palette.
	*/
	int colorPaletteSize;
	
	/** 
	 * follows the bmp file header data, take note that although the
	 * format is the one accomodating new bitmap, the old bitmap format
	 * is also supported by same structures, but the structural order 
	 * within the following differs from the headers.
	 */

	/** The file type - must be 'BM' for a valid bitmap.
	*/
	word fileType;

	/** The file size in bytes.
	*/
	dword fileSize;

	/** hotspot coordinates - the 2 reserved words.
	*/
	word Xhot;
	
	/** hotspot coordinates - the 2 reserved words.
	*/
	word Yhot;
	
	/** The offset where the bitmap data starts in the data stream.
	*/
	dword bitmapOffset;

	/* follows the bitmap header data */

	/** The size of the bmp header in bytes.
	*/
	dword headerSize; // if 12 the BMP version is OLD
	/** The image width in pixels.
	*/
	dword width;
	/** The image height in pixels or scanlines.
	*/
	dword height;
	/** The image planes - allways 1.
	*/
	word planes;
	/** Number of bits per pixel.
	*/
	word bitsPerPixel;
	/** The type of compression - allways NONE = 0.
	*/
	dword compression;
	/** The size of the bitmap in bytes.
	*/
	dword bitmapSize;
	/** The horizontal resolution in pixels per meter.
	*/
	long horizResolution;
	/** Vertical resolution in pixel per meter.
	*/
	long vertResolution;
	/** Number of colors in the image.
	*/
	dword colorsUsed;
	/** Minimum number of important colors.
	*/
	dword colorsImportant;

	/* follow some fields for the OS2 bmp file format */
	word units; // units of measurement
	word reserved;
	word recording; // recording algorithm
	word rendering; // halftoning algorithm
	dword size1; // size value 1
	dword size2; // size value 2
	dword identifier; // for aplications use

	/**
	* The folowing method initializes the fields with default values.
	*/
	void initValuesToDefault();
};

#endif //_BMPIMAGE_H_


