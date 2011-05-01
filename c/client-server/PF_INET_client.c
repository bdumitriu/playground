#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int main()
{
        struct sockaddr_in server_address;
        int sock;
        struct hostent *hostinfo;
        char *message;

        sock = socket(PF_INET, SOCK_STREAM, 0);
        if (sock < 0)
        {
	perror("socket");
	exit(EXIT_FAILURE);
        }

        server_address.sin_family = AF_INET;
        server_address.sin_port = htons(6300);
        hostinfo = gethostbyname("blackwizard.bdumitriu.ro");
        if (hostinfo == NULL)
        {
	fprintf(stderr, "Unknown host %s.\n", "blackwizard.bdumitriu.ro");
	exit(EXIT_FAILURE);
        }
        server_address.sin_addr = * ((struct in_addr *) hostinfo->h_addr);

        if (connect(sock, (struct sockaddr *) &server_address, sizeof(server_address)) < 0)
        {
	perror("connect");
	exit(EXIT_FAILURE);
        }

        message[0] = 'a';
        while (1)
        {
	printf("Message (^C to quit): ");
                scanf("%s", message);
	if (write(sock, message, strlen(message)+1) < 0)
                {
	        perror("write");
	        exit(EXIT_FAILURE);
	}
	printf("Message sent to server.\n");
        }
        
        close(sock);
        return 0;
}
