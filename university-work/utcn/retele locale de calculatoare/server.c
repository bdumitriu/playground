#include <stdio.h>
#include <stdlib.h> 
#include <errno.h> 
#include <string.h>
 
#include <sys/types.h> 
#include <netinet/in.h> 
#include <sys/socket.h> 
#include <sys/wait.h> 

#define MYPORT 3490    /* numar port va fi completat corespunzator */

#define BACKLOG 10     /* numar de conexiuni  concurente*/

int main(void)
{
	int sockfd, new_fd;		/* asculta la sock_fd,  o noua conexiune la new_fd */
	struct sockaddr_in my_addr;	/* info despre adresa proprie */
	struct sockaddr_in their_addr;	/* info despre adresa conectorului */
	int sin_size;

	if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
	{
		perror("socket");
		exit(1);
	}

	my_addr.sin_family = AF_INET;         /* host byte order */
	my_addr.sin_port = htons(MYPORT);     /* short, network byte order */
        my_addr.sin_addr.s_addr = INADDR_ANY; /* adresa de IP */
        bzero(&(my_addr.sin_zero), 8);        /* zero  în rest */

	if (bind(sockfd, (struct sockaddr *)&my_addr, sizeof(struct sockaddr)) == 1)
	{
		perror("bind");
		exit(1);
	}

	if (listen(sockfd, BACKLOG) == -1)
	{
		perror("listen");
		exit(1);
	}

	while(1)
	{
		sin_size = sizeof(struct sockaddr_in);

		if ((new_fd = accept(sockfd, (struct sockaddr *)&their_addr,   &sin_size)) == -1)
		{
			perror("accept");
			continue;
		}

		printf("server: conexiunea de la from %s\n", inet_ntoa(their_addr.sin_addr));

		if (!fork())
		{
			if (send(new_fd, "Hello, world!\n", 14, 0) == -1)
				perror("send");
			close(new_fd);
			exit(0);
		}

		close(new_fd);

		while(waitpid(-1, NULL, WNOHANG) > 0);
	}
}
