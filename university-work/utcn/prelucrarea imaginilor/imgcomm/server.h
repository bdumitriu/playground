#ifndef SERVER_H
#define SERVER_H

#include <string.h>
#include <stdio.h>
#include <windows.h>

#include "defs.h"
#include "md5checksum.h"

#include "Encoder.h"

/**
 * Default port on which the main server
 * command thread runs.
 */
static int DEFAULT_COMMAND_PORT = 3125;

/**
 * Default port on which the main server
 * data thread runs.
 */
static int DEFAULT_DATA_PORT = 3126;

/**
 * Maximum length of the queue of pending connections.
 */
static int BACKLOG = 20;

/**
 * This class provides the server functionality of the
 * image transmission system. In order to create a Server object
 * several things have to be set up. These are:
 * <ul>
 * <li>the number of images sent in a single communication</li>
 * <li>the size of each of these images in bytes (they are assumed to
 * be of the same size)</li>
 * <li>the type of each of these images (they are assumed to
 * be of the same type)</li>
 * <li>the memory address from which the images are read</li>
 * </ul>
 *
 * The specified memory address is expected to be a pointer at
 * an area of unsigned chars formatted like this:
 * <ul>
 * <li>4 bytes - an integer used for synchronization with the camera
 * driver (see below for further explanations)</li>
 * <li>imgSize bytes - the first image</li>
 * <li>imgSize bytes - the second image</li>
 * <li>...</li>
 * <li>imgSize bytes - the n-th image</li>
 * </ul>
 * The integer used for synchronization should be either 0 or 1.
 * The synchronization method used by the server is the following:
 * when a client makes a request for images, the server waits until
 * this integer has a value of 1. As soon as this condition is met,
 * the server starts reading the memory, assuming that it is filled
 * with valid data. Then it sends this data to the client. At the end
 * of this process, the server sets the value of this integer to 0,
 * thus making sure that an external process will modify the data in
 * the memory before another read occurs.
 * <br /><br />
 * This class should be used as follows: a Server object should be
 * created and then the server should be started. Before starting
 * the server, the data port can be set manually (otherwise, the
 * DEFAULT_DATA_PORT from server.h will be used). If and when so
 * desired, the server can be stopped by calling its stop method.
 * <br /><br />
 * The server runs on two distinct ports. They are called the command
 * and the data port. The command port is used for light <nobr>client/server</nobr>
 * traffic and is used by the client to find various information about
 * the server (such as the number of images it sends, their size, type,
 * etc.). The data port is used for heavy <nobr>client/server</nobr> traffic since
 * it is the port on which all the image data is sent to the client.
 * Both the main command thread and the main data thread spawn a new
 * thread to handle each of the connected clients.
 *
 * @author Bogdan Dumitriu
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 */
class Server
{
public:
	/**
	 * Builds a new Server object which packs & sends <code>nrImages</code> images of
	 * size <code>imgSize</code> and of type <code>bmpType</code> from memory buffer
	 * pointed to by <code>address</code>. It uses stdout to print error messages and
	 * DEFAULT_COMMAND_PORT from server.h as the main command thread port number.
	 *
	 * @param nrImages the number of images the server reads, packs and sends in one shot
	 * @param imgSize the size (in bytes) of each of these images
	 * @param bmpType the type of each of these images
	 * @param address the address of the memory buffer to use (see class description
	 *	for more information on what the server expects to find in this buffer)
	 */
	Server(int nrImages, int imgSize, BitmapType bmpType, char* address);

	/**
	 * Builds a new Server object which packs & sends <code>nrImages</code> images of
	 * size <code>imgSize</code> and of type <code>bmpType</code> from memory buffer
	 * pointed to by <code>address</code>. It prints error messages to <code>logFile</code>
	 * and uses DEFAULT_COMMAND_PORT from server.h as the main command thread port number.
	 * The <code>logFile</code> should be opened for writing before creating the Server
	 * object and closed after deleting it.
	 *
	 * @param nrImages the number of images the server reads, packs and sends in one shot
	 * @param imgSize the size (in bytes) of each of these images
	 * @param bmpType the type of each of these images
	 * @param address the address of the memory buffer to use (see class description
	 *	for more information on what the server expects to find in this buffer)
	 * @param logFile the file to print error messages to
	 */
	Server(int nrImages, int imgSize, BitmapType bmpType, char* address, FILE* logFile);
	
	/**
	 * Builds a new Server object which packs & sends <code>nrImages</code> images of
	 * size <code>imgSize</code> and of type <code>bmpType</code> from memory buffer
	 * pointed to by <code>address</code>. It uses stdout to print error messages and
	 * <code>port</code> as the main command thread port number.
	 *
	 * @param nrImages the number of images the server reads, packs and sends in one shot
	 * @param imgSize the size (in bytes) of each of these images
	 * @param bmpType the type of each of these images
	 * @param address the address of the memory buffer to use (see class description
	 *	for more information on what the server expects to find in this buffer)
	 * @param port the port on which the main command thread should run
	 */
	Server(int nrImages, int imgSize, BitmapType bmpType, char* address, u_short port);

	/**
	 * Builds a new Server object which packs & sends <code>nrImages</code> images of
	 * size <code>imgSize</code> and of type <code>bmpType</code> from memory buffer
	 * pointed to by <code>address</code>. It prints error messages to <code>logFile</code>
	 * and uses <code>port</code> as the main command thread port number. The <code>logFile</code>
	 * should be opened for writing before creating the Server object and closed after deleting it.
	 *
	 * @param nrImages the number of images the server reads, packs and sends in one shot
	 * @param imgSize the size (in bytes) of each of these images
	 * @param bmpType the type of each of these images
	 * @param address the address of the memory buffer to use (see class description
	 *	for more information on what the server expects to find in this buffer)
	 * @param port the port on which the main command thread should run
	 * @param logFile the file to print error messages to
	 */
	Server(int nrImages, int imgSize, BitmapType bmpType, char* address, u_short port, FILE* logFile);

	/**
	 * This is the destructor of the Server class.
	 */
	~Server();

	/**
	 * Starts both the main command and the main data threads.
	 *
	 * @return 0 is all goes well and -1 if anything goes wrong.
	 *	Look for error messages in the log file to see what went
	 *	wrong, if the case.
	 */
	int start();

	/**
	 * Stops the server.
	 */
	void stop();

	/**
	 * Sets the data port to <code>dataPort</code>. This methods only has effect
	 * if the server isn't running, which means either {@link #start} hasn't
	 * been called yet or {@link #start} and {@link #stop} have both been called.
	 *
	 * @param dataPort the new value of the data port
	 *
	 * @return old value of data port if successful, -1 if it fails
	 *	due to the reasons explained above.
	 */
	int setDataPort(int dataPort);

	/**
	 * Returns the port on which the main data thread of the server runs.
	 *
	 * @return the port on which the main data thread of the server runs.
	 */
	int getDataPort() {return dataPort;}

	/**
	 * Returns the number of images sent by the server to clients.
	 *
	 * @return the number of images sent by the server to clients.
	 */
	int getNrImages() {return nr;}

	/**
	 * Returns the size (in bytes) of the images sent by the server to clients.
	 *
	 * @return the size (in bytes) of the images sent by the server to clients.
	 */
	int getImgSize() {return size;}

	/**
	 * Returns the type of the images sent by the server to clients.
	 *
	 * @return the type of the images sent by the server to clients.
	 */
	BitmapType getBitmapType() {return bmpType;}

	/**
	 * Returns the address the server reads the images from.
	 *
	 * @return the address the server reads the images from.
	 */
	char* getAddress() {return address;}

	/**
	 * Returns the port on which the main command thread of the server runs.
	 *
	 * @return the port on which the main command thread of the server runs.
	 */
	u_short getPort() {return port;}

	/**
	 * Private method which is made public because of implementation difficulties.
	 * Please do not call this method.
	 */
	void printError();

private:
	/** which port to run command thread on */
	u_short port;		

	/** which port to run data thread on */
	u_short dataPort;	

	/** number of images to read each time */
	int nr;			

	/** image size in pixels */
	int size;

	/** bitmap type */
	BitmapType bmpType;

	/** memory address where to start reading from */
	char* address;

	/** the log file */
	FILE* log;

	/** the main server command socket */
	SOCKET servSock;

	/** the main server data socket */
	SOCKET dataSock;

	/** handle to main command thread */
	HANDLE mainCmdTh;

	/** handle to main data thread */
	HANDLE mainDataTh;

	/** are we running or not */
	bool running;

	/* some private methods follow */

	/** Internal initializations */
	void init();
};

#endif // SERVER_H
