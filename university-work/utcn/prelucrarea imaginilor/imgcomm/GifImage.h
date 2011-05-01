
#ifndef _GIFIMAGE_H_
#define _GIFIMAGE_H_

#include "BinaryReader.h"
#include "LZWCompressor.h"

/** The gif signature.
*/
#define GIF_SIGNATURE 0x00464947 //little endian for "GIF"

/** The versions available.
*/
enum Version
{
	gif87a	= 0x00613738,
	gif89a	= 0x00613938,
	notGif	= 0x00000000,
};

/** Special codes used to delimit major blocks.
*/
enum SpecialCodes
{
	ExtensionIntroducer		= 0x21,
	ImageSeparator			= 0x2C,
	GraphicControlBlockSize	= 0x04,
	PlainTextBlockSize		= 0x0C,
	AplicationBlockSize		= 0x0B,
	BlockTerminator			= 0x00,
	GifTrailer				= 0x3B,
};

/** Specify the extensions available.
*/
enum ExtensionLabels
{
	GraphicControlLabel	= 0xF9,
	CommentLabel		= 0xFE,
	PlainTextLabel		= 0x01,
	ExtensionLabel		= 0xFF,
};

/** The disposal method.
*/
enum DisposalMethod
{
	NoDisposalSpecified,
	DoNotDispose,
	RestoreBackgroundColor,
	RestoreToPrevious,
	ToBeDefined,
};

/** Useful structure to read the color components out of a gif file.
*/
typedef struct gif_rgb
{
	byte red;
	byte green;
	byte blue;
} GIF_RGB;

 /**
 * Used to contain the control extension fields.<br /><br />
 * Author Tudor Marian <br />
 *
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * created: mai, 2nd, 2003 <br />
 *
 * @author	Tudor Marian
 */
class ControlExtension
{
public:

	/**
	* Creates an object.
	*/
	ControlExtension(){};

	/**
	* Destroys the object.
	*/
	~ControlExtension(){};

	/* for conveniance purposes the attributes are public */

	/** A 3 bit disposal method, 0-3 valid values, 4-7 to be defined.
	*/
	byte disposalMethod;

	/** A user input flag, if true user input is expected.
	*/
	bool userInputFlag;

	/** The transparancy flag, if true a transparent index is to be considered.
	*/
	bool transparentColorFlag;

	/**
	* Used with the user input flag, delay of 1/100 of a second before
	* continuing with the processing of the data stream.
	*/
	word delayTime;

	/** The index of the transparent color.
	*/
	byte transparentColorIndex;
	
	/** The block terminator, because of the graphic control block size.
	*/
	byte blockTerminator;
};


 /**
 * The class describes a gif image file. It supports ONLY ONE gif image file,
 * thus only one global color TABLE and only one local color table.
 * Warning: no special extensions are taken under consideration, neither
 * the plain text nor the comment or aplication extensions, only the 
 * graphic control extension is considered. <br /><br />
 *
 * Author Tudor Marian <br />
 *
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * created: mai, 2nd, 2003 <br />
 *
 * @author	Tudor Marian
 */
class GifImage
{

public:
	/** Creates a gif image object.
	*/
	GifImage();

	/**
	* Creates a gif image object.
	*
	* @param	image	the serialized form of the gif file, it will be used
	*	to extract gif relevand data.
	*/
	GifImage(ByteBuffer* image);
	
	/** Destroys the gif image object.
	*/
	~GifImage();

	/**
	* Reads up the data from the image, parses it's relevant info and
	* stores itinto local attributes.
	*
	* @return	false	if the reading process failed, true otherwise.
	*/
	bool init();

	/**
	* Reads up the data from the image, parses it's relevant info and
	* stores itinto local attributes.
	*
	* @param	image	the serialized form of the gif image file used
	*	as data source for the parsing.
	* @return	false	if the reading process failed, true otherwise.
	*/
	bool init(ByteBuffer* image);

	/**
	* Sets up the image data.
	*
	* @param	image	the serialized form of the source gif file.
	*/
	void setImage(ByteBuffer* image) {this->image = image;};

	/**
	* Provides the aspect ratio of the image.
	*
	* @return	the aspect ratio of the image.
	*/
	int aspectRatio() {return ((pixelAspectRatio + 15) / 64);};

	
	/**
	* Returns the gif's version.
	*
	* @return	the gif's version.
	*/
	dword getVersion() {return version;};

	/**
	* Returns the gif image's screen width.
	*
	* @return	the screen width.
	*/
	word getScreenWidth() {return screenWidth;};

	/**
	* Returns the gif image's screen height.
	*
	* @return	the screen height.
	*/
	word getScreenHeight() {return screenHeight;};
	
	/**
	* Checks if the gif has a global color table or not.
	*
	* @retun	true if the gif file has a global color table, 
	*	false otherwise.
	*/
	bool hasGlobalColorTable() {return globalColorTableFlag;};

	/**
	* Returns the color resolution.
	*
	* @return	the color resolution of the gif file.
	*/
	byte getColorResolution() {return colorResolution;};

	/**
	* Checks if the global color table is sorted or not.
	*
	* @return	true if the global color table is sorted
	*	false otherwise.
	*/
	bool isGlobalColorTableSorted() {return globalSortFlag;};

	/**
	* Returns the global color table size - in elements, not bytes.
	*
	* @return	the size of the global color table.
	*/
	int getGlobalColorTableSize() {return globalColorTableSize;};

	/**
	* Returns the index of the background color in the color table.
	*
	* @return	the index of the background color into the color table.
	*/
	byte getBackgroundTableIndex() {return backgroundColorIndex;};
	
	/**
	* Returns the global color table as an array of red-green-blue triplet
	* packed into a <code>gif_rgb</code> structure.
	*
	* @return	the global color table if one exists, NULL, otherwise.
	*/
	gif_rgb* getGlobalColorTable() {return globalColorTable;};

	/**
	* Returns the image's left position towards the screen.
	*
	* @return	the image's left position.
	*/
	word getImageLeftPosition() {return imageLeftPosition;};

	/**
	* Returns the image's top position towards the screen.
	*
	* @return	the image's top position.
	*/
	word getImageTopPosition() {return imageTopPosition;};

	/**
	* Returns the image's effective width.
	*
	* @return	the gif image's width.
	*/
	word getImageWidth() {return imageWidth;};

	/**
	* Returns the image's effective height.
	*
	* @return	the gif image's height.
	*/
	word getImageHeight() {return imageHeight;};

	/**
	* Checks the presence of a local color table.
	*
	* @return	true if a local color table is present, false
	*	otherwise.
	*/
	bool hasLocalColorTable() {return localColorTableFlag;};

	/**
	* Checks if the image is interlaced or not.
	*
	* @return	true if the image is interlaced, false otherwise.
	*/
	bool isInterlaced() {return interlaceFlag;};

	/**
	* Checks if the local color table is sorted or not.
	*
	* @return	true if the local color table is sorted
	*	false otherwise.
	*/
	bool isLocalColorTableSorted() {return localSortFlag;};

	/**
	* Returns the size of the local color table, if one is present
	* 0 otherwise.
	*
	* @return	the size of the local color table.
	*/
	int getLocalColorTableSize() {return localColorTableSize;};

	/**
	* Returns the local color table as an array of red-green-blue triplet
	* packed into a <code>gif_rgb</code> structure.
	*
	* @return	the local color table if one exists, NULL, otherwise.
	*/
	gif_rgb* getLocalColorTable() {return localColorTable;};

	/**
	* Returns the minimum size of the LZW compression algorithm code
	* to start encoding/decoding with. The actual code is increased with
	* one unit.
	*
	* @return the minimum code size of the LZW algorithm.
	*/
	byte getLZWMinimumCodeSize() {return lzwMinimumCodeSize;};
	
	/**
	* Returns the compressed image data buffer.
	*
	* @return	the compressed image data buffer.
	*/
	ByteBuffer* getCompressedImageData() {return compressedImageData;};

	/**
	* Returns the control extension object read, if any is present into
	* the gif file.
	*
	* @return	the control extension object.
	*/
	ControlExtension* getControlExtension() {return controlExtension;};

	/**
	* Decompress the compressed image data using the gif version of the
	* LZW algorithm.
	*
	* @return	the decompressed bytes into a buffer object.
	*/
	ByteBuffer* decompressImageData();

	/**
	* Serialize the gif data into a data byte block to be ready for
	* being outputted to a gif file.
	*
	* @return	the serialized form of this gif image object.
	*/
	ByteBuffer* serialize();

	/**
	* Writes the compressed data stream to the file <code>filename</code>.
	* It is used for debugging purposes only.
	*
	* @param	filename	the name of the file to store the compressed data
	*	into.
	*/
	void writeCompressedDataToFile(char* filename);

private:

	/**
	* Reads the image descriptor.
	*/
	void readImageDescriptor();

	/**
	* Reads the graphic control extension.
	*/
	void readGraphicControlExtension();

	/**
	* Reads the graphic control extension.
	*/
	void readCommentExtension();

	/**
	* Reads the plain text extension.
	*/
	void readPlainTextExtension();

	/**
	* Reads the application extension.
	*/
	void readApplicationExtension();

	/**
	* Reads the compressed image data - it is found as a discrete
	* number of blocks.
	*/
	void readImageData();


	/** The serialized form of the image.
	*/
	ByteBuffer* image;

	/** 
	* A usefull wrapper object over the seriaized data ment to read
	* signifiant chunks of data.
	*/
	BinaryReader* reader;

	/* Follows GIF header information */

	/** The gif version, the signature must be "GIF" = 0x00464947
	*/
	dword version;

	/* Logical Screen Descriptor data */

	/** The logical screen width.
	*/
	word screenWidth;
	
	/** The logical screen height.
	*/
	word screenHeight;
	
	// packed fields follow

	/** If true a global color table will follow.
	*/
	bool globalColorTableFlag;
	
	/** Number of bits per primary color, the read value is -1.
	*/
	byte colorResolution;

	/** If true the color table is sorted according to the color importance.
	*/
	bool globalSortFlag;

	/** The size of the global color table in number of entries.
	*/
	int globalColorTableSize;

	/** Background color index.
	*/
	byte backgroundColorIndex;

	/** The pixel aspect ratio.
	*/
	byte pixelAspectRatio;

	/**
	* The global color table, if any. It's size in bytes should 
	* be 3x2^(size of global color table + 1).
	*/
	gif_rgb* globalColorTable;

	/* the image descriptor */

	/** Image separator.
	*/
	byte imageSeparator;

	/** Image left position.
	*/
	word imageLeftPosition;

	/** Image top position.
	*/
	word imageTopPosition;

	/** Image width.
	*/
	word imageWidth;

	/** Image height.
	*/
	word imageHeight;

	// folow some packed fields.

	/** If true a local color table will follow.
	*/
	bool localColorTableFlag;

	/** If true the image is interlaced after a four-pass interlace pattern.
	*/
	bool interlaceFlag;

	/** 
	* If true the colors from the local color table are ordered by 
	* decreasing importance.
	*/
	bool localSortFlag;

	/** Local color table size in entries (2^ (field value + 1)).
	*/
	int localColorTableSize;

	/** The local color table.
	*/
	gif_rgb* localColorTable;

	/* table based image data */

	/**
	* The number of bits/code the LZW compression algorithm
	* starts with (it will start at this value + 1 up to 12 bits/code).
	*/
	byte lzwMinimumCodeSize;

	/** 
	* The compressed data buffer - will be read from blocks & 
	* inserted into a buffer.
	*/
	ByteBuffer*	compressedImageData;

	/** Graphic control extension - only if version is gif89a.
	*/
	ControlExtension* controlExtension;

	// comment follows - will be skipped
	// plain text follows - will be skipped
	// application extension follows - will be skipped
	
	/* GIF trailer should be read */
};

#endif //_GIFIMAGE_H_



