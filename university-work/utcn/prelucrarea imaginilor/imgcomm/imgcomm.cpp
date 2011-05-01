#include <windows.h>
#include <iostream.h>
#include <conio.h>

#include "defs.h"
#include "server.h"
#include "client.h"
#include "md5checksum.h"

typedef union _Conv
{
	int number;
	char buffer[4];
} Conv;

DWORD WINAPI serverThread(void* arg);
DWORD WINAPI clientThread(void* arg);

static const imgSize = 2359350;
static const nrImg = 1;

Server* srv = NULL;

int main(int argc, char* argv[])
{
	DWORD dwTid;
	HANDLE sThread, cThread;

	sThread = CreateThread(NULL, 0, serverThread, NULL, 0, &dwTid);
	cThread = CreateThread(NULL, 0, clientThread, NULL, 0, &dwTid);

	if ((sThread == NULL) || (cThread == NULL))
	{
		printf("Error creating thread.\n");
		exit(1);
	}

	while (getch() != 'e');

	srv->stop();

	return 0;
}

DWORD WINAPI serverThread(void* arg)
{
	char* buffer = (char*) malloc(
		sizeof(char) * (imgSize * nrImg + 4));

	srv = new Server(nrImg, imgSize, Bit24, buffer);

	Conv x;
	x.number = 0;
	memcpy(buffer, x.buffer, 4);

	srv->start();

	char* file = new char[20];
	FILE* f;
	int idx = 0;
	while (true)
	{
		/* fill buffer */
		for (int i = 0; i < nrImg; i++)
		{
			idx++;
			if (idx == 7)
				idx = 1;

			sprintf(file, "c:\\%d.bmp", idx);
			if ((f = fopen(file, "rb")) == NULL)
			{
				printf("ERROR OPENING FILE: %s\n", file);
				return 1;
			}
			for (int j = 0; j < imgSize; j++)
				fscanf(f, "%c", &buffer[imgSize*i+j+4]);

			fclose(f);
		}
		x.number = 1;
		memcpy(buffer, x.buffer, 4);

		memcpy(x.buffer, buffer, sizeof(int));
		while (x.number != 0)
		{
			Sleep(1);
			memcpy(x.buffer, buffer, sizeof(int));
		}
	}
}

DWORD WINAPI clientThread(void* arg)
{
	Client* clnt = new Client("127.0.0.1");
	if (clnt->Connect() == false)
	{
		printf("FAILED TO CONNECT.\n");
		return 0;
	}

	int x = 6;
	int idx = 0;
	FILE* f;
	char* file = new char[20];
	int clntImgSize = clnt->getImgSize();
	int clntNrImages = clnt->getNrImages();
	while (x > 0)
	{
		x--;
		unsigned char* buffer;
		if ((buffer = clnt->getImages()) != NULL)
		{
			for (int i = 0; i < clntNrImages; i++)
			{
				idx++;
				sprintf(file, "d:\\%d.bmp", idx);
				if ((f = fopen(file, "wb")) == NULL)
				{
					printf("ERROR OPENING FILE: %s\n", file);
					exit(1);
				}
				for (int j = 0; j < clntImgSize; j++)
					fprintf(f, "%c", buffer[clntImgSize*i+j]);

				fclose(f);
			}
			free(buffer);
		}
		else
		{
			printf("BUFFER ERROR.\n");
			return 1;
		}
	}

	clnt->disconnect();
	delete clnt;

	return 0;
}
