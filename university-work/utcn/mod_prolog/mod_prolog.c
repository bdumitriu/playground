/*
 * Author: Bogdan DUMITRIU
 * Date:   02.08.2001
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"
#include "http_main.h"
#include "util_script.h"
#include "ap_config.h"
#include "http_log.h"

/*
 * The port on which the Sicstus Prolog server is supposed to be
 * running.
 */
#define PORT 22222

/*
 * The address of the host on which Sicstus Prolog server is
 * supposed to be running.
 */
#define ADDR "127.0.0.1"

/*
 * The maximum number of characters which the module is
 * able to read from the server in one bunch.
 */
#define MAX 35000

/*
 * The path to Sicstus, including the program name.
 */
#define SICSTUS_WITH_PATH "/usr/local/SICStus/bin/sicstus"

/*
 * The Sicstus Prolog program name, without path.
 */
#define SICSTUS "sicstus"

/*
 * The path to the Sicstus server file, including its name.
 */
#define SICSTUS_SERVER "/var/www/htdocs/s.pl"

/*
 * This function appends the _from_ string to the _to_ string.
 */
void append(char *to, char *from)
{
        int i, m, n;

        m = strlen(to);
        n = strlen(from);
        for (i = 0; i < n; i++)
		to[m+i] = from[i];
        to[m+n] = '\0';
}

/*
 * This function opens a connections to a (supposed) Sicstus Prolog 
 * server on ADDR:PORT (see macro definitions) and returns the connection 
 * handler.
 */
int initialize_socket()
{
        int sock;
        long i;
        struct sockaddr_in server;
        
        // create the socket
        sock = socket(PF_INET, SOCK_STREAM, 0);
        if (sock < 0)
        	return -1;

        // create the record for identifing the server
        server.sin_family = AF_INET;
        server.sin_port = htons(PORT);
        if (inet_aton(ADDR, &server.sin_addr) == 0)
		return -2;

        // connect to server
        i = 0;
        while (i < 100000)
        {
                if (connect(sock, (struct sockaddr *) &server, sizeof(server)) < 0)
	        	i++;
		else
	        	i = 200000;
        }
        if (i != 200000)
		return -3;
        
        return sock;
}

/*
 * Sends _write_buffer_ to the server and ouputs the result received
 * from it to the requester using Apache's ap_rputs() function.
 */
int process_line(int sock, char *write_buffer, request_rec *r)
{
	int length, i, n;
	char read_buffer[MAX];

	// we add an extra '\n' character as the server seem to need
	// two '\n's instead of one.
	length = strlen(write_buffer);
	write_buffer[length] = '\n';
	write_buffer[length+1] = '\0';

        if (write(sock, write_buffer, strlen(write_buffer)+1) < 0)
		return -1;

        if ((strstr(write_buffer, "halt.") != NULL) &&
		(strlen(write_buffer) <= 7))
		return 0;

        read_buffer[0] = '\0';
        while (strstr(read_buffer, "\n!#%&(@$^*)\n") == '\0')
        {
		ap_rputs(read_buffer, r);
		n = read(sock, read_buffer, MAX);
		if (n < 0)
	        	return -2;
		read_buffer[n] = '\0';
        }
        read_buffer[strlen(read_buffer)-12] = '\0';
	ap_rputs(read_buffer, r);
        ap_rputs("\n", r);
        read_buffer[0] = '\0';
        
        return 0;
}

/*
 * This function initializes the connection to the Prolog server,
 * procceses the _f_ file and then closes the connection.
 */
int handle_file(FILE *f, request_rec *r)
{
        char line[MAX];
        int sock, n, length, stats;

        sock = initialize_socket(r);
        
        if (sock == -1)
        {
		ap_rputs("Could not create socket.<br><br>", r);
		return HTTP_SERVICE_UNAVAILABLE;
        }
        
        if (sock == -2)
        {
		ap_rputs("Could not locate server host.<br><br>", r);
		return HTTP_SERVICE_UNAVAILABLE;
        }
        
        if (sock == -3)
        {
		ap_rputs("Could not connect to Prolog server.<br><br>", r);
		return HTTP_SERVICE_UNAVAILABLE;
        }

        while (!feof(f))
        {
		fgets(line, MAX, f);
		stats = process_line(sock, line, r);	
		if (stats == -1)
		{
			ap_rputs("Write error occured (server stopped?)<br><br>", r);
		        return HTTP_SERVICE_UNAVAILABLE;
		}
		if (stats == -2)
		{
	        	ap_rputs("Read error occured (server stopped?)<br><br>", r);
		        return HTTP_SERVICE_UNAVAILABLE;
		}
        }
	
        close(sock);
        return OK;
}

/*
 * This is the function called by Apache.
 */
static int prolog_handler(request_rec *r)
{
        char *filename = r->filename;
        FILE *f;
        int fk, status, http_status = OK;
        char *ps[4];

        f = fopen(filename, "r");

        if (f == NULL)
        {
		return HTTP_NOT_FOUND;
        }

        r->content_type = "text/html";
        ap_send_http_header(r);

	// try to start the Prolog server
        if (!r->header_only)
        {
		fk = fork();
		if (fk == -1)
		{
	        	ap_rputs("Couldn't fork!", r);
		        http_status = HTTP_SERVICE_UNAVAILABLE;
		}
		if (fk == 0)
		{
	        	ps[0] = SICSTUS;
		        ps[1] = "-l";
		        ps[2] = SICSTUS_SERVER;
	        	ps[3] = NULL;
		        if (execv(SICSTUS_WITH_PATH, ps) < 0)
		        {
				ap_rputs("Couldn't start server.", r);
				http_status = HTTP_SERVICE_UNAVAILABLE;
		        }
		}
		else
		{
	        	http_status = handle_file(f, r);
		}
        }

        fclose(f);

        return http_status;
}

/*
 * An element of this array should be a 2-dimensional
 * array like: {"Apache_handler_name", module_function}.
 * The Apache_handler_name is the name that will be
 * used in the httpd.conf file like:
 * 	...
 * 	AddHandler Apache_handler_name .file_extension
 *	...
 * (meaning that all files with the specified extension
 * will be handled by this handler), while module_function
 * should be the name of the function Apache should call
 * when this handler is supposed to take on. The last
 * element in the array should always be {NULL,NULL}.
 */
static const handler_rec prolog_handlers[] = {
        {"prolog", prolog_handler},
        {NULL, NULL}
};

/*
 * This module represents the means by which a module
 * makes itself part of the Apache web server. The 8th
 * value (namely prolog_handlers in this case) should be
 * the name of a variable of type handler_rec. The numbers
 * in brackets (like [#...]) represent the level in
 * the Apache hierarchy at which the specified handlers
 * are called (if value is other than NULL). This level
 * stuff is rather complex and Apache documentation
 * should be read for thorough understanding
 * (www.apache.org (or something like that) might be
 * a good place to start).
 */
module MODULE_VAR_EXPORT prolog_module = {
	STANDARD_MODULE_STUFF,
	NULL,		/* module initializer */
	NULL,		/* create per-dir config structures */
	NULL,		/* merge per-dir config structures */
	NULL,		/* create per-server config structures */
	NULL,		/* merge per-server config structures */
	NULL,		/* table of config file commands */
	prolog_handlers,/* [#8] MIME-typed-dispatched handlers */
	NULL,		/* [#1] URI to filename translation */
	NULL,		/* [#4] validate user id from request */
	NULL,		/* [#5] check if the user is ok _here_ */
	NULL,		/* [#3] check address by host address */
	NULL,		/* [#6] determine MIME types */
	NULL,		/* [#7] pre-run fixups */
	NULL,		/* [#9] log a transaction */
	NULL,		/* child_init */
	NULL,		/* child_exit */
	NULL		/* [#0] post read-request */
};