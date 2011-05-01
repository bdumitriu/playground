#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>

int main()
{
	struct sockaddr_un server_address;
	int sock;
	socklen_t server_size;
	char *message;

	sock = socket(PF_LOCAL, SOCK_STREAM, 0);
	if (sock < 0)
	{
		perror("socket");
		exit(EXIT_FAILURE);
	}

	server_address.sun_family = AF_LOCAL;
	strncpy(server_address.sun_path, "/tmp/sicstus.sock", sizeof(server_address.sun_path));
	server_size = SUN_LEN(&server_address);
	
	if (connect(sock, (struct sockaddr *) &server_address, server_size) < 0)
	{
		perror("connect");
		exit(EXIT_FAILURE);
	}
	
	message = "Hello world!";
	if (write(sock, message, strlen(message)+1) < 0)
	{
		perror("write");
		exit(EXIT_FAILURE);
	}
}
