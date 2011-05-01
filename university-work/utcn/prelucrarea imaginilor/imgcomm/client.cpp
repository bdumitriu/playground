#include "client.h"

typedef union _Conv
{
	int number;
	char buffer[4];
} Conv;

Client::Client(char* serverHost)
{
	log = stdout;

	servHost = new char[strlen(serverHost)+1];
	if (servHost == NULL)
	{
		fprintf(log, "client: memory allocation error.\n");
		exit(1);
	}

	strcpy(servHost, serverHost);
	this->port = DEFAULT_CLIENT_PORT;

	init();
}

Client::Client(char* serverHost, FILE* logFile)
{
	log = logFile;

	servHost = new char[strlen(serverHost)+1];
	if (servHost == NULL)
	{
		fprintf(log, "client: memory allocation error.\n");
		exit(1);
	}

	strcpy(servHost, serverHost);
	this->port = DEFAULT_CLIENT_PORT;

	init();
}

Client::Client(char* serverHost, u_short port)
{
	log = stdout;

	servHost = new char[strlen(serverHost)+1];
	if (servHost == NULL)
	{
		fprintf(log, "client: memory allocation error.\n");
		exit(1);
	}

	strcpy(servHost, serverHost);
	this->port = port;

	init();
}

Client::Client(char* serverHost, u_short port, FILE* logFile)
{
	log = logFile;

	servHost = new char[strlen(serverHost)+1];
	if (servHost == NULL)
	{
		fprintf(log, "client: memory allocation error.\n");
		exit(1);
	}

	strcpy(servHost, serverHost);
	this->port = port;

	init();
}

Client::~Client()
{
	if (connected == true)
		disconnect();
	delete servHost;
}

void Client::init()
{
	connected = false;
}

bool Client::Connect()
{
	/* initialize the windows socket system */
	WORD version = MAKEWORD(2, 2);
	WSADATA wsaData;
	WSAStartup(version, &wsaData);

	struct sockaddr_in servAddr;

	/* connect to the server command port */

	clientSock = socket(PF_INET, SOCK_STREAM, IPPROTO_IP);

	if (clientSock == INVALID_SOCKET)
	{
		fprintf(log, "client: create socket failed.\n");
		
		/* stop the windows socket system */
		WSACleanup();

		return false;
	}

	servAddr.sin_family = AF_INET;
	servAddr.sin_port = htons(port);
	servAddr.sin_addr.S_un.S_addr = inet_addr(servHost);

	if (connect(clientSock, (struct sockaddr*) &servAddr, sizeof(struct sockaddr)) == SOCKET_ERROR)
	{
		fprintf(log, "client: failed to connect to server command port (%s:%d).\n",
			servHost, port);
		
		/* stop the windows socket system */
		WSACleanup();
		
		return false;
	}

	Conv tmp;

	/* get information from server */
	send(clientSock, "NUMB", 4, 0);
	if (_recv(clientSock, &(tmp.buffer[0]), 4, 0) == -1)
	{
		/* stop the windows socket system */
		WSACleanup();
		
		return false;
	}
	nr = tmp.number;

	send(clientSock, "SIZE", 4, 0);
	if (_recv(clientSock, &(tmp.buffer[0]), 4, 0) == -1)
	{
		/* stop the windows socket system */
		WSACleanup();
		
		return false;
	}
	size = tmp.number;

	send(clientSock, "BITM", 4, 0);
	if (_recv(clientSock, &(tmp.buffer[0]), 4, 0) == -1)
	{
		/* stop the windows socket system */
		WSACleanup();
		
		return false;
	}
	if (tmp.number == 0)
		bmpType = Bit8;
	else
		bmpType = Bit24;

	send(clientSock, "PORT", 4, 0);
	if (_recv(clientSock, &(tmp.buffer[0]), 4, 0) == -1)
	{
		/* stop the windows socket system */
		WSACleanup();
		
		return false;
	}
	dataPort = tmp.number;

	send(clientSock, "DONE", 4, 0);

	closesocket(clientSock);

	/* connect to the server data port */

	clientSock = socket(PF_INET, SOCK_STREAM, IPPROTO_IP);

	if (clientSock == INVALID_SOCKET)
	{
		fprintf(log, "client: create socket failed.\n");
		
		/* stop the windows socket system */
		WSACleanup();
		
		return false;
	}

	servAddr.sin_family = AF_INET;
	servAddr.sin_port = htons(dataPort);
	servAddr.sin_addr.S_un.S_addr = inet_addr(servHost);

	if (connect(clientSock, (struct sockaddr*) &servAddr, sizeof(struct sockaddr)) == SOCKET_ERROR)
	{
		fprintf(log, "client: failed to connect to server data port (%s:%d).\n",
			servHost, dataPort);
		
		/* stop the windows socket system */
		WSACleanup();
		
		return false;
	}

	connected = true;

	return true;
}

void Client::disconnect()
{
	if (connected == false)
		return;

	/* disconnect from server */
	send(clientSock, "DONE", 4, 0);
	closesocket(clientSock);

	/* stop the windows socket system */
	WSACleanup();

	connected = false;
}

int Client::getNrImages()
{
	if (connected == false)
		return -1;
	else
		return nr;
}

int Client::getImgSize()
{
	if (connected == false)
		return -1;
	else
		return size;
}

BitmapType Client::getBitmapType()
{
	if (connected == false)
		return Bit8;
	else
		return bmpType;
}

unsigned char* Client::getImages()
{
	if (connected == false)
		return NULL;

	char* buffer = NULL;
	long decImgSize;
	byte* decImg = (byte*) malloc(sizeof(byte) * size * nr);
	if (decImg == NULL)
	{
		fprintf(log, "client: memory allocation error.\n");
		exit(1);
	}

	char* md5 = (char*) malloc(sizeof(char) * 32);
	if (md5 == NULL)
	{
		fprintf(log, "client: memory allocation error.\n");
		exit(1);
	}

	bool errorOccured = false;
	Conv tmp;

	send(clientSock, "SEND", 4, 0);
	for (int i = 0; i < nr; i++)
	{
		if (_recv(clientSock, &(tmp.buffer[0]), sizeof(int), 0) == -1)
		{
			free(decImg);
			return NULL;
		}

		if (buffer != NULL)
		{
			free(buffer);
			buffer = NULL;
		}
		buffer = (char*) malloc(sizeof(char) * tmp.number);
		if (buffer == NULL)
		{
			fprintf(log, "client: memory allocation error.\n");
			exit(1);
		}

		if (_recv(clientSock, buffer, tmp.number, 0) == -1)
		{
			free(decImg);
			return NULL;
		}
		if (_recv(clientSock, md5, 32, 0) == -1)
		{
			free(decImg);
			return NULL;
		}

		byte* _decImg = decImg + i * size;
		byte* __decImg;
		if (bmpType == Bit8)
		{
			Encoder::decodeGrayscaleImage((byte*) buffer, tmp.number,
				&__decImg, &decImgSize);
		}
		else
		{
			Encoder::decodeHighColorImage((byte*) buffer, tmp.number,
				&__decImg, &decImgSize);
		}

		memcpy(_decImg, __decImg, decImgSize);
		free(__decImg);

		char* computed_md5 = MD5Checksum::getMD5(_decImg, decImgSize);
		if (strncmp(md5, computed_md5, 32) != 0)
		{
			errorOccured = true;
		}
	}

	if (errorOccured == true)
	{
		free(decImg);
		return NULL;
	}
	else
		return decImg;
}

int Client::_recv(SOCKET s, char* buf, int len, int flags)
{
	int numBytes = 0;
	while (numBytes < len)
	{
		int nb = recv(s, buf + numBytes, len - numBytes, 0);
		if (nb == SOCKET_ERROR)
		{
			fprintf(log, "client: read socket error.\n");
			return -1;
		}
		else if (nb == 0)
		{
			fprintf(log, "client: socket closed by server.\n");
			return -1;
		}
		else
			numBytes += nb;
	}

	return numBytes;
}

void Client::printError()
{
	int err = WSAGetLastError();
	switch (err)
	{
		case WSANOTINITIALISED:
		{
			fprintf(log, "A successful WSAStartup call must occur before using this function.");
			break;
		}
		case WSAENETDOWN:
		{
			fprintf(log, "The network subsystem or the associated service provider has failed.");
			break;
		}
		case WSAEAFNOSUPPORT:
		{
			fprintf(log, "The specified address family is not supported.");
			break;
		}
		case WSAEINPROGRESS:
		{
			fprintf(log, "A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function.");
			break;
		}
		case WSAEMFILE:
		{
			fprintf(log, "No more socket descriptors are available.");
			break;
		}
		case WSAENOBUFS:
		{
			fprintf(log, "No buffer space is available. The socket cannot be created.");
			break;
		}
		case WSAEPROTONOSUPPORT:
		{
			fprintf(log, "The specified protocol is not supported.");
			break;
		}
		case WSAEPROTOTYPE:
		{
			fprintf(log, "The specified protocol is the wrong type for this socket.");
			break;
		}
		case WSAESOCKTNOSUPPORT:
		{
			fprintf(log, "The specified socket type is not supported in this address family.");
			break;
		}
		default:
		{
			fprintf(log, "Unknwown error: %d.", err);
			break;
		}
	}
	fprintf(log, "\n");
}
