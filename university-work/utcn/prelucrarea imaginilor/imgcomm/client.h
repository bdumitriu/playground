#ifndef CLIENT_H
#define CLIENT_H

#include <string.h>
#include <stdio.h>
#include <windows.h>

#include "defs.h"
#include "md5checksum.h"

#include "Encoder.h"

/**
 * Default port to which the client tries to connect
 * if the port parameter isn't specified when creating the
 * client object.
 */
static int DEFAULT_CLIENT_PORT = 3125;

/**
 * This class provides the client functionality of the
 * image transmission system. A typical usage would be
 * the following:
 * <pre>
 * [ create a new client object ]
 * [ call the Connect method ]
 * [
 * 	call getNrImages, getImgSize and getBitmapType methods
 * 	to find out how many images the server sends, the size
 * 	of each of these images and their type
 * ]
 * [ repeatedly call getImages to get image(s) from the server ]
 * [ call the disconnect method ]
 * </pre>
 * None of the {@link #getNrImages}, {@link #getImgSize}, {@link #getBitmapType}
 * and {@link #getImages} will return a valid result without a previous
 * *successful* call to {@link #Connect}.
 *
 * @author Bogdan Dumitriu
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 */
class Client
{
public:
	/**
	 * Builds a new Client object which will connect to the specified
	 * host. This Client will use DEFAULT_CLIENT_PORT from client.h as
	 * the port number and stdout for printing error messages. The port
	 * refers to the port on which the main server command thread runs
	 * (see {@link Server} class for details).
	 *
	 * @param serverHost the IP address of the server in dotted decimal notation
	 */
	Client(char* serverHost);

	/**
	 * Builds a new Client object which will connect to the specified
	 * host. This Client will use DEFAULT_CLIENT_PORT from client.h as
	 * the port number and <code>logFile</code> for printing error messages.
	 * <code>logFile</code> is expected to be open for writing. <code>logFile</code>
	 * will not be closed automatically by any of the Client object's methods
	 * (including the destructor). The user is expected to close it. The port
	 * refers to the port on which the main server command thread runs
	 * (see {@link Server} class for details).
	 *
	 * @param serverHost the IP address of the server in dotted decimal notation
	 * @param logFile the file error messages will be printed to
	 */
	Client(char* serverHost, FILE* logFile);

	/**
	 * Builds a new Client object which will connect to the specified
	 * host. This Client will use <code>port</code> as the port number
	 * and stdout for printing error messages. The port
	 * refers to the port on which the main server command thread runs
	 * (see {@link Server} class for details).
	 *
	 * @param serverHost the IP address of the server in dotted decimal notation
	 * @param port the port number of the server
	 */
	Client(char* serverHost, u_short port);

	/**
	 * Builds a new Client object which will connect to the specified
	 * host. This Client will use <code>port</code> as
	 * the port number and <code>logFile</code> for printing error messages.
	 * <code>logFile</code> is expected to be open for writing. <code>logFile</code>
	 * will not be closed automatically by any of the Client object's methods
	 * (including the destructor). The user is expected to close it. The port
	 * refers to the port on which the main server command thread runs
	 * (see {@link Server} class for details).
	 *
	 * @param serverHost the IP address of the server in dotted decimal notation
	 * @param port the port number of the server
	 * @param logFile the file error messages will be printed to
	 */
	Client(char* serverHost, u_short port, FILE* logFile);

	/**
	 * This is the destructor of the Client class.
	 */
	~Client();

	/**
	 * Connects to the server and retrieves useful information
	 * including data port, number of images sent by server, image
	 * size and image type. This method should always be called
	 * before making calls to any of other methods.
	 * <br /><br />
	 * Note: the method's name starts with an uppercase letter
	 * (as opposed to all others) because of the fact that there
	 * is a connect function in the socket interface and the two
	 * identical names caused a conflict.
	 *
	 * @return true after a successful connect and false otherwise.
	 */
	bool Connect();

	/**
	 * Disconnects the client from the server. The method should
	 * be called when the communication ends.
	 */
	void disconnect();

	/**
	 * Note: this method will only return a valid result after a previous
	 * *successful* call to {@link #Connect}.
	 * 
	 * @return the number of images the server sends or -1 if
	 * client is not connected yet.
	 */
	int getNrImages();

	/**
	 * Note: this method will only return a valid result after a previous
	 * *successful* call to {@link #Connect}.
	 *
	 * @return the size of the each of the images the server sends or -1 if
	 * client is not connected yet.
	 */
	int getImgSize();

	/**
	 * Note: this method will only return a valid result after a previous
	 * *successful* call to {@link #Connect}.
	 *
	 * @return the type of the images the server sends. If a previous
	 * call to connect has not been made, the method returns Bit8 by default.
	 */
	BitmapType getBitmapType();

	/**
	 * @return true if client is already connected and false otherwise.
	 */
	bool isConnected() {return connected;}

	/**
	 * Fetches and returns a new set of images from the server. The
	 * images are returned as a pointer to a memory buffer which
	 * contains them. This buffer contains {@link #getNrImages} images, each
	 * of size {@link #getImgSize} and of type {@link #getBitmapType}.
	 * <br /><br />
	 * If, for example, nrImages is 2, imgSize is 1024*768 and bitmapType
	 * is Bit8, it means that this buffer will be 1024*768*2 bytes
	 * (unsigned char's) long, with bytes 0 through 1024*768 - 1 containing
	 * the first grayscale image and bytes 1024*768 through 1024*768*2 - 1
	 * containing the second grayscale image.
	 * <br /><br />
	 * If the client is not connected yet, the method returns NULL. Check
	 * the log for reasons. This method also returns NULL if a transmission
	 * error is detected (this is done by internally computing the MD5 of
	 * each transmitted image). You can tell the difference bewteen the
	 * two types of NULL by calling {@link #isConnected}. If it returns true,
	 * the NULL symbolizes a transmission error. If it returns false, the
	 * NULL symbolizes that the client is not connected.
	 * <br /><br />
	 * !!! IMPORTANT NOTE !!!<br />
	 * The memory area returned by this method should be freed once
	 * you no longer need it as it is not freed internally (this can't
	 * be done because the class has no way of knowing when this
	 * should happen).<br />
	 * !!! END OF IMPORTANT NOTE !!!
	 *
	 * @return a pointer to a memory buffer containing the images fetched from
	 *	the server
	 */
	unsigned char* getImages();

private:
	/** the *IP* address on which the server runs */
	char* servHost;

	/** the port on which the server main command thread runs */
	u_short port;

	/** socket with server */
	SOCKET clientSock;

	/*
	 * the following four members are only valid
	 * after a successful call to connect
	 */

	/** number of images sent by server */
	int nr;

	/** size of each of the images sent by server */
	int size;

	/** type of the images sent by server */
	BitmapType bmpType;

	/** the port on which the server data thread runs */
	int dataPort;

	/** connection status */
	bool connected;

	/** the log file */
	FILE* log;

	/** internal initializations */
	void init();

	/**
	 * Recv wrapper which makes sure that all bytes are received.
	 */
	int _recv(SOCKET s, char* buf, int len, int flags);

	/**
	 * Method used for printing error messages during debugging.
	 */
	void printError();
};

#endif // CLIENT_H
