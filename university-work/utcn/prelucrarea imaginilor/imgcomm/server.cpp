#include "server.h"

typedef struct _CmdThreadArg
{
	SOCKET localSock;
	SOCKET remoteSock;
	struct sockaddr_in remoteAddr;
	Server* server;
	FILE* log;
} CmdThreadArg;

typedef union _Conv
{
	int number;
	char buffer[4];
} Conv;

void printError();
DWORD WINAPI mainCommandThread(LPVOID _server);
DWORD WINAPI commandThread(LPVOID _client);
DWORD WINAPI mainDataThread(LPVOID _arg);
DWORD WINAPI dataThread(LPVOID _arg);

CRITICAL_SECTION criticalSection;
bool criticalSectionInited = false;

Server::Server(int nrImages, int imgSize, BitmapType bmpType, char* address)
{
	nr = nrImages;
	size = imgSize;
	this->bmpType = bmpType;
	this->address = address;
	this->port = DEFAULT_COMMAND_PORT;
	log = stdout;
	init();
}

Server::Server(int nrImages, int imgSize, BitmapType bmpType, char* address, FILE* logFile)
{
	nr = nrImages;
	size = imgSize;
	this->bmpType = bmpType;
	this->address = address;
	this->port = DEFAULT_COMMAND_PORT;
	log = logFile;
	init();
}

Server::Server(int nrImages, int imgSize, BitmapType bmpType, char* address, u_short port)
{
	nr = nrImages;
	size = imgSize;
	this->bmpType = bmpType;
	this->address = address;
	this->port = port;
	log = stdout;
	init();
}

Server::Server(int nrImages, int imgSize, BitmapType bmpType, char* address, u_short port, FILE* logFile)
{
	nr = nrImages;
	size = imgSize;
	this->bmpType = bmpType;
	this->address = address;
	this->port = port;
	log = logFile;
	init();
}

Server::~Server()
{
	if (criticalSectionInited == true)
	{
		criticalSectionInited = false;
		DeleteCriticalSection(&criticalSection);
	}
}

void Server::init()
{
	dataPort = DEFAULT_DATA_PORT;
	mainCmdTh = NULL;
	mainDataTh = NULL;
	running = false;

	if (criticalSectionInited == false)
	{
		criticalSectionInited = true;
		InitializeCriticalSection(&criticalSection);
	}
}

int Server::start()
{
	struct sockaddr_in servAddr;

	running = true;

	/* initialize the windows socket system */
	WORD version = MAKEWORD(2, 2);
	WSADATA wsaData;
	WSAStartup(version, &wsaData);

	servSock = socket(AF_INET, SOCK_STREAM, IPPROTO_IP);

	if (servSock == INVALID_SOCKET)
	{
		fprintf(log, "server: create command socket failed.\n");
		return -1;
	}

	servAddr.sin_family = AF_INET;
        servAddr.sin_port = htons(port);
        servAddr.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(servSock, (struct sockaddr*) &servAddr, sizeof(struct sockaddr)) == SOCKET_ERROR)
	{
		fprintf(log, "server: bind command socket failed.\n");
		return -1;
	}

	if (listen(servSock, BACKLOG) == SOCKET_ERROR)
	{
		fprintf(log, "server: listen call failed on command socket.\n");
		return -1;
	}

	CmdThreadArg* server = new CmdThreadArg;
	if (server == NULL)
	{
		fprintf(log, "server: memory allocation error.\n");
		exit(0);
	}

	server->localSock = servSock;
	server->server = this;
	server->log = log;

	LPDWORD lpThreadId = new DWORD;
	mainCmdTh = CreateThread(NULL, 0, mainCommandThread, server, 0, lpThreadId);
	if (mainCmdTh == NULL)
	{
		fprintf(log, "server: create command thread failed.\n");
		return -1;
	}

	dataSock = socket(AF_INET, SOCK_STREAM, IPPROTO_IP);

	if (dataSock == INVALID_SOCKET)
	{
		fprintf(log, "server: create data socket failed.\n");
		stop();
		return -1;
	}

	servAddr.sin_family = AF_INET;
        servAddr.sin_port = htons(dataPort);
        servAddr.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(dataSock, (struct sockaddr*) &servAddr, sizeof(struct sockaddr)) == SOCKET_ERROR)
	{
		fprintf(log, "server: bind data socket failed.\n");
		stop();
		return -1;
	}

	if (listen(dataSock, BACKLOG) == SOCKET_ERROR)
	{
		fprintf(log, "server: listen call failed on data socket.\n");
		stop();
		return -1;
	}

	CmdThreadArg* data = new CmdThreadArg;
	if (data == NULL)
	{
		fprintf(log, "server: memory allocation error.\n");
		stop();
		exit(0);
	}

	data->localSock = dataSock;
	data->server = this;
	data->log = log;

	mainDataTh = CreateThread(NULL, 0, mainDataThread, data, 0, lpThreadId);
	if (mainDataTh == NULL)
	{
		fprintf(log, "server: create data thread failed.\n");
		stop();
		return -1;
	}

	return 0;
}

void Server::stop()
{
	if (mainCmdTh != NULL)
		TerminateThread(mainCmdTh, 0);
	if (mainDataTh != NULL)
		TerminateThread(mainDataTh, 0);

	closesocket(servSock);

	/* stop the windows socket system */
	WSACleanup();

	mainCmdTh = NULL;
	mainDataTh = NULL;
	running = false;
}

DWORD WINAPI mainCommandThread(LPVOID _arg)
{
	struct sockaddr_in clientAddr;
	SOCKET clientSock;
	CmdThreadArg* arg = (CmdThreadArg*) _arg;
	SOCKET servSock = arg->localSock;
	FILE* log = arg->log;

	while (true)
	{
		int addrSize = sizeof(struct sockaddr);
		clientSock = accept(servSock, (struct sockaddr*) &clientAddr, &addrSize);

		if (clientSock == INVALID_SOCKET)
		{
			fprintf(log, "server main command thread: invalid socket with client.\n");
			continue;
		}

		CmdThreadArg* threadArg = new CmdThreadArg;
		if (threadArg == NULL)
		{
			fprintf(log, "server main command thread: memory allocation error.\n");
			exit(0);
		}

		threadArg->localSock = servSock;
		threadArg->remoteSock = clientSock;
		memcpy(&(threadArg->remoteAddr), &clientAddr, sizeof(struct sockaddr_in));
		threadArg->server = arg->server;
		threadArg->log = log;

		LPDWORD lpThreadId = new DWORD;
		CreateThread(NULL, 0, commandThread, threadArg, 0, lpThreadId);
	}
}

DWORD WINAPI commandThread(LPVOID _arg)
{
	CmdThreadArg* arg = (CmdThreadArg*) _arg;
	FILE* log = arg->log;

	char* buffer = new char[5];
	int val, numBytes, nb;
	Conv tmp;
	fd_set sockSet;

	FD_ZERO(&sockSet);
	FD_SET(arg->remoteSock, &sockSet);

	do
	{
		if ((val = select(0, &sockSet, NULL, NULL, NULL)) == SOCKET_ERROR)
		{
			fprintf(log, "server command thread: select socket error.\n");
			return 1;
		}

		numBytes = 0;
		while (numBytes < 4)
		{
			nb = recv(arg->remoteSock, buffer + numBytes, 4 - numBytes, 0);
			if (nb == SOCKET_ERROR)
			{
				fprintf(log, "server command thread: read socket error.\n");
				return 1;
			}
			else if (nb == 0)
			{
				fprintf(log, "server command thread: socket closed by client.\n");
				return 0;
			}
			else
				numBytes += nb;
		}

		buffer[numBytes] = '\0';
		if (strcmp(buffer, "NUMB") == 0)
		{
			tmp.number = arg->server->getNrImages();
		}
		else if (strcmp(buffer, "SIZE") == 0)
		{
			tmp.number = arg->server->getImgSize();
		}
		else if (strcmp(buffer, "BITM") == 0)
		{
			tmp.number = arg->server->getBitmapType();
		}
		else if (strcmp(buffer, "PORT") == 0)
		{
			tmp.number = arg->server->getDataPort();
		}
		else if (strcmp(buffer, "DONE") == 0)
		{
			return 0;
		}
		else
		{
			tmp.number = -1;
		}
		send(arg->remoteSock, &(tmp.buffer[0]), sizeof(int), 0);
	}
	while (true);
}

DWORD WINAPI mainDataThread(LPVOID _arg)
{
	struct sockaddr_in clientAddr;
	SOCKET clientSock;
	CmdThreadArg* arg = (CmdThreadArg*) _arg;
	SOCKET servSock = arg->localSock;
	FILE* log = arg->log;

	while (true)
	{
		int addrSize = sizeof(struct sockaddr);
		clientSock = accept(servSock, (struct sockaddr*) &clientAddr, &addrSize);

		if (clientSock == INVALID_SOCKET)
		{
			fprintf(log, "server main data thread: invalid socket with client.\n");
			continue;
		}

		CmdThreadArg* threadArg = new CmdThreadArg;
		if (threadArg == NULL)
		{
			fprintf(log, "server main data thread: memory allocation error.\n");
			exit(0);
		}

		threadArg->localSock = servSock;
		threadArg->remoteSock = clientSock;
		memcpy(&(threadArg->remoteAddr), &clientAddr, sizeof(struct sockaddr_in));
		threadArg->server = arg->server;
		threadArg->log = log;

		LPDWORD lpThreadId = new DWORD;
		CreateThread(NULL, 0, dataThread, threadArg, 0, lpThreadId);
	}
}

DWORD WINAPI dataThread(LPVOID _arg)
{
	CmdThreadArg* arg = (CmdThreadArg*) _arg;
	FILE* log = arg->log;
	Server* server = arg->server;

	int nr = server->getNrImages();
	int imgSize = server->getImgSize();
	BitmapType bmpType = server->getBitmapType();
	byte* addr = (byte*) server->getAddress();

	char* buffer = new char[5];
	int val, numBytes, nb;
	Conv tmp;
	fd_set sockSet;

	byte* encImg = NULL;
	long encImgSize;

	FD_ZERO(&sockSet);
	FD_SET(arg->remoteSock, &sockSet);

	do
	{
		if ((val = select(0, &sockSet, NULL, NULL, NULL)) == SOCKET_ERROR)
		{
			fprintf(log, "server data thread: select socket error.\n");
			return 1;
		}

		numBytes = 0;
		while (numBytes < 4)
		{
			nb = recv(arg->remoteSock, buffer + numBytes, 4 - numBytes, 0);
			if (nb == SOCKET_ERROR)
			{
				fprintf(log, "server data thread: read socket error.\n");
				return 1;
			}
			else if (nb == 0)
			{
				fprintf(log, "server data thread: socket closed by client.\n");
				return 0;
			}
			else
				numBytes += nb;
		}

		buffer[numBytes] = '\0';
		if (strcmp(buffer, "SEND") == 0)
		{
			EnterCriticalSection(&criticalSection);

			/* wait until driver fetches new image from camera */
			memcpy(tmp.buffer, addr, sizeof(int));
			while (tmp.number != 1)
			{
				Sleep(1);
				memcpy(tmp.buffer, addr, sizeof(int));
			}

			for (int i = 0; i < nr; i++)
			{
				if (bmpType == Bit8)
				{
					Encoder::encodeGrayscaleImage(
						addr + i * imgSize + sizeof(int),
						imgSize, &encImg, &encImgSize);
				}
				else
				{
					Encoder::encodeHighColorImage(
						addr + i * imgSize + sizeof(int),
						imgSize, &encImg, &encImgSize);
				}

				tmp.number = encImgSize;
				send(arg->remoteSock, &(tmp.buffer[0]), sizeof(int), 0);
				send(arg->remoteSock, (char*) encImg, encImgSize, 0);
				send(arg->remoteSock,
					MD5Checksum::getMD5(addr + i * imgSize + sizeof(int),
					imgSize), 32, 0);
			}

			tmp.number = 0;
			memcpy(addr, tmp.buffer, sizeof(int));

			LeaveCriticalSection(&criticalSection);
		}
		else if (strcmp(buffer, "DONE") == 0)
		{
			return 0;
		}
		else
		{
			tmp.number = -1;
			send(arg->remoteSock, &(tmp.buffer[0]), sizeof(int), 0);
		}
	}
	while (true);
}

int Server::setDataPort(int dataPort)
{
	if (running == true)
		return -1;

	u_short oldPort = this->dataPort;
	this->dataPort = dataPort;

	return oldPort;
}

void Server::printError()
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