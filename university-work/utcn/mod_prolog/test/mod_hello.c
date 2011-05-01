#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"
#include "http_main.h"
#include "util_script.h"
#include "ap_config.h"
#include "http_log.h"
#include <stdio.h>

static int hello_handler(request_rec *r)
{
	char *filename = r->filename;
	FILE *f;
	char c;
	int test = 1;
	
	f = fopen(filename, "r");
	
	if (f == NULL)
	{
		return HTTP_NOT_FOUND;
	}
	
	r->content_type = "text/html";
	ap_send_http_header(r);
	
	if (!r->header_only)
	{
		c = fgetc(f);
		while (!feof(f))
		{
			if (c == '#')
			{
				test = 0;
			}
			if (c == '\n')
			{
				test = 1;
			}
			if (test)
			{
				ap_rputc(c, r);
			}
			c = fgetc(f);
		}
	}
	
	fclose(f);
	
	return OK;
}

static const handler_rec hello_handlers[] = {
	{"hello", hello_handler},
	{NULL}
};

module MODULE_VAR_EXPORT hello_module = {
	STANDARD_MODULE_STUFF,
	NULL,				/* module initializer */
	NULL,				/* create per-dir config structures */
	NULL,				/* merge per-dir config structures */
	NULL,				/* create per-server config structures */
	NULL,				/* merge per-server config structures */
	NULL,				/* table of config file commands */
	hello_handlers,			/* [#8] MIME-typed-dispatched handlers */
	NULL,				/* [#1] URI to filename translation */
	NULL,				/* [#4] validate user id from request */
	NULL,				/* [#5] check if the user is ok _here_ */
	NULL,				/* [#3] check address by host address */
	NULL,				/* [#6] determine MIME types */
	NULL,				/* [#7] pre-run fixups */
	NULL,				/* [#9] log a transaction */
	NULL,				/* child_init */
	NULL,				/* child_exit */
	NULL				/* [#0] post read-request */
};