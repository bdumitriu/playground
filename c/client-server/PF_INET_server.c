#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>

int main()
{
        int sock, new_sock;
        struct sockaddr_in name, client_address;
        fd_set active_fd_set, read_fd_set;
        int i, nbytes;
        char buffer[500];
        size_t size;

        sock = socket(PF_INET, SOCK_STREAM, 0);
        if (sock < 0)
        {
	perror("socket");
	exit(EXIT_FAILURE);
        }

        name.sin_family = AF_INET;
        name.sin_port = htons(6300);
        name.sin_addr.s_addr = htonl(INADDR_ANY);

        if (bind(sock, (struct sockaddr *) &name, sizeof(name)) < 0)
        {
	perror("bind");
	exit(EXIT_FAILURE);
        }

        if (listen(sock, 1) < 0)
        {
	perror("listen");
	exit(EXIT_FAILURE);
        }

        FD_ZERO(&active_fd_set);
        FD_SET(sock, &active_fd_set);

        while (1)
        {
	read_fd_set = active_fd_set;
	if (select(FD_SETSIZE, &read_fd_set, NULL, NULL, NULL) < 0)
	{
	        perror("select");
	        exit(EXIT_FAILURE);
	}
	
	for (i = 0; i < FD_SETSIZE; i++)
	{
	        if (FD_ISSET(i, &read_fd_set))
	        {
		if (i == sock)
		{
		        size = sizeof(client_address);
		        new_sock = accept(sock,
			          (struct sockaddr *) &client_address,
			          &size);
		        if (new_sock < 0)
		        {
			perror("accept");
			exit(EXIT_FAILURE);
		        }
		        fprintf(stderr,
			"Server: connect from host %s, port %hd.\n",
			inet_ntoa(client_address.sin_addr),
			ntohs(client_address.sin_port));
		        FD_SET(new_sock, &active_fd_set);
		}
		else
		{
		        nbytes = read(i, buffer, 500);
		        if (nbytes < 0)
		        {
			perror("read");
			exit(EXIT_FAILURE);
		        }
		        else if (nbytes == 0)
		        {
			close(i);
			FD_CLR(i, &active_fd_set);
		        }
		        else
		        {
			fprintf(stderr, "Server: got message: `%s'\n", buffer);
		        }
		}
	        }
	}
        }
        
        return 0;
}