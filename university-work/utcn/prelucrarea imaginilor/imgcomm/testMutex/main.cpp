#include <windows.h>
#include <stdio.h>
#include <conio.h>

DWORD WINAPI serverThread(void* arg);
DWORD WINAPI clientThread(void* arg);

CRITICAL_SECTION CriticalSection;

int main(int argc, char* argv[])
{
	DWORD dwTid;
	HANDLE sThread, cThread;

	InitializeCriticalSection(&CriticalSection);

	sThread = CreateThread(NULL, 0, serverThread, NULL, 0, &dwTid);
	cThread = CreateThread(NULL, 0, clientThread, NULL, 0, &dwTid);

	if ((sThread == NULL) || (cThread == NULL))
	{
		printf("Error creating thread.\n");
		exit(1);
	}

	while (getch() != 'e');

	DeleteCriticalSection(&CriticalSection);

	return 0;
}

DWORD WINAPI serverThread(void* arg)
{
	EnterCriticalSection(&CriticalSection);
	printf("1: in critical section\n");
	Sleep(3000);
	LeaveCriticalSection(&CriticalSection);

	return 0;
}

DWORD WINAPI clientThread(void* arg)
{
	EnterCriticalSection(&CriticalSection);
	printf("2: in critical section\n");
	LeaveCriticalSection(&CriticalSection);

	return 0;
}

