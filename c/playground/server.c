#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>

int main()
{
	struct sockaddr_un name, client_address;
	int sock, new_sock;
	size_t size;
	socklen_t client_size;
	char buffer[20];

	sock = socket(PF_LOCAL, SOCK_STREAM, 0);
	if (sock < 0)
	{
		perror("socket");
		exit(EXIT_FAILURE);
	}

	name.sun_family = AF_LOCAL;
	strncpy(name.sun_path, "/tmp/sicstus.sock", sizeof(name.sun_path));
	size = SUN_LEN(&name);
	
	if (bind(sock, (struct sockaddr *) &name, size) < 0)
	{
		perror("bind");
		exit(EXIT_FAILURE);
	}
	
	if (listen(sock, 50) < 0)
	{
		perror("listen");
		exit(EXIT_FAILURE);
	}
	
	new_sock = accept(sock, (struct sockaddr *) &client_address, &client_size);
	if (new_sock < 0)
	{
		perror("accept");
		exit(EXIT_FAILURE);
	}
	
	if (read(new_sock, buffer, 20) < 0)
	{
		perror("read");
		exit(EXIT_FAILURE);
	}
	
	fprintf(stderr, "Server: got message: '%s'\n", buffer);
}
