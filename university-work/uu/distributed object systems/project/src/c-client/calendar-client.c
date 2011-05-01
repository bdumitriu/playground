/**
 * A CORBA client written in C to prove interoperability.
 *
 * Author:	Bogdan Dumitriu
 * Date:	April 10, 2005
 * Version:	0.1
 *
 * This code is heavily based on the one in the tutorial found here:
 * http://www.gnome.org/projects/ORBit2/orbit-docs.tar.gz
 */

#include <stdio.h>
#include <signal.h>
#include <orbit/orbit.h>
#include <ORBitservices/CosNaming.h>

#include "Calendar.h"

static CORBA_ORB global_orb = CORBA_OBJECT_NIL;

/**
 * Tests @ev for any exception.
 */
gboolean
raised_exception(CORBA_Environment *ev)
{
	return (ev->_major != CORBA_NO_EXCEPTION);
}

/**
 * In case of any exception this operation will abort the process.
 */
void 
abort_if_exception(CORBA_Environment *ev, const char* msg)
{
	if (raised_exception(ev))
	{
		g_error("%s %s", msg, CORBA_exception_id(ev));
		CORBA_exception_free(ev);
		abort();
	}
}

/**
 * Returns the length of a NULL terminated string vector.
 */
static
guint
id_vec_len(char *id_vec[])
{ 
	gint i = 0;
	for (i = 0; id_vec[i]; i++);
	return i;
}

/**
 * Resolves default name-service, usually given to application as
 * command line argument "-ORBInitRef NameService=IOR:0100000028..",
 * or since release 2.8.0 corbalocs in form of URL can be used, e.g.:
 * "-ORBInitRef NameService=corbaloc:iiop:HOSTNAME:PORT/NameService%00"
 */
CosNaming_NamingContext
get_name_service(CORBA_ORB orb, CORBA_Environment *ev)
{
	CORBA_char *str = NULL;
	CORBA_Object ref = (CORBA_Object)
		CORBA_ORB_resolve_initial_references(orb, "NameService", ev);

	if (raised_exception(ev))
	{
		return CORBA_OBJECT_NIL;
	}

        return (CosNaming_NamingContext) ref;
}

/**
 * Resolves and returns an object reference with unique name @id_vec
 * at @name_service. @id_vec is a NULL terminated list of strings
 * (CORBA_char*). If an error occures @ev points to the exception
 * object on return.
 */
CORBA_Object
name_service_resolve(CosNaming_NamingContext name_service,
	gchar *id_vec[],
	CORBA_Environment *ev)
{
	CORBA_Object retval = CORBA_OBJECT_NIL;
	gint i = 0;
	gint len = id_vec_len(id_vec);

	// allocate a CosNaming::Name (sequence of CosNaming::NameComponent)
	CosNaming_Name *name = CosNaming_Name__alloc();

	name->_buffer = CORBA_sequence_CosNaming_NameComponent_allocbuf(len);
	name->_maximum = len;
	name->_length = 0;

	// relinquish ownership of the NameComponent to the
	// sequence. When CORBA_free is called on it later, the
	// NameComponent will be freed
	CORBA_sequence_set_release(name, TRUE);

	// iterate components of name and create sub-context (directory) if needed
	for (i = 0; i < len; i++)
	{
		name->_length = i+1;
		name->_buffer[i].id = CORBA_string_dup(id_vec[i]);
		name->_buffer[i].kind = CORBA_string_dup("");
	}

	//g_print("debug::1");
	retval = CosNaming_NamingContext_resolve(name_service, name, ev);
	//g_print("debug::2");

	if (raised_exception(ev))
	{
		CORBA_free(name);
		return CORBA_OBJECT_NIL;
	}

	return retval;
}

/**
 * Used as signal handler to invoke the CORBA_ORB_shutdown()
 * function, which will terminate the process' main loop.
 */
static
void
client_shutdown(int sig)
{
	CORBA_Environment local_ev[1];
	CORBA_exception_init(local_ev);

	if (global_orb != CORBA_OBJECT_NIL)
	{
		CORBA_ORB_shutdown(global_orb, FALSE, local_ev);
		abort_if_exception(local_ev, "caught exception:");
	}
}

/**
 * Inits ORB @orb using @argv arguments for configuration. For each
 * ORBit options consumed from vector @argv the counter of @argc_ptr
 * will be decremented. Signal handler is set to call
 * echo_client_shutdown function in case of SIGINT and SIGTERM
 * signals. If error occures @ev points to exception object on
 * return.
 */
static
void
client_init(int *argc_ptr, char *argv[], CORBA_ORB *orb, CORBA_Environment *ev)
{
	// register signal handler for SIGINT & SIGTERM
	signal(SIGINT,  client_shutdown);
	signal(SIGTERM, client_shutdown);
         
	// create Object Request Broker (ORB)
         
	(*orb) = CORBA_ORB_init(argc_ptr, argv, "orbit-local-mt-orb", ev);
	if (raised_exception(ev))
	{
		return;
	}
}

/**
 * Does the actual work.
 */
static
void
client_run (interoperability_Calendar service, CORBA_Environment *ev)
{
	gint i;
	char name[1024+1];
	char pass[1024+1];
	interoperability_Appointments* apps;
	interoperability_Appointment app;
	interoperability_Date date;

	g_print("\nUsing this client you can get a listing of your appointments.\n");
	g_print("Please type your login name and password:\n");

	g_print("Login name: ");
	fgets(name, 1024, stdin);
	// chop the newline off
	name[strlen(name)-1] = '\0';

	g_print("Password: ");
	fgets(pass, 1024, stdin);
	// chop the newline off
	pass[strlen(pass)-1] = '\0';

	apps = interoperability_Calendar_getAppointments(service, name, pass, ev);

	if (raised_exception(ev))
	{
		return;
	}

	g_print("\nThe appointments in your calendar are:\n\n");
	for (i = 0; i < apps->_length; i++)
	{
		app = apps->_buffer[i];
		g_print("Location    : %s\n", app.location);
		g_print("Description : %s\n", app.description);
		date = app.startDate;
		g_print("From        : %.2d-%.2d-%d %.2d:%.2d:%.2d\n", date.day, date.month, date.year, date.hour, date.minute, date.second);
		date = app.endDate;
		g_print("To          : %.2d-%.2d-%d %.2d:%.2d:%.2d\n", date.day, date.month, date.year, date.hour, date.minute, date.second);
		if (app.isGroupAppointment == 1)
		{
			g_print("Type        : group appointment\n");
		}
		else
		{
			g_print("Type        : single appointment\n");
		}
		g_print("\n");
	}
}

/**
 * Releases @servant object and finally destroys @orb. If an error
 * occures @ev points to the exception object on return.
 */
static
void
client_cleanup(CORBA_ORB orb, CORBA_Object service, CORBA_Environment *ev)
{
	// release managed object
	CORBA_Object_release(service, ev);
	if (raised_exception(ev))
	{
		return;
	}

	// tear down the ORB
	if (orb != CORBA_OBJECT_NIL)
	{
		CORBA_ORB_destroy(orb, ev);
		if (raised_exception(ev))
		{
			return;
		}
	}
}

/**
 * The main function.
 */
int main(int argc, char* argv[])
{
	interoperability_Calendar service = CORBA_OBJECT_NIL;
	CosNaming_NamingContext name_service = CORBA_OBJECT_NIL;

	gchar *id[] = {"calendar", NULL};

	CORBA_Environment ev[1];
	CORBA_exception_init(ev);

	client_init(&argc, argv, &global_orb, ev);
	abort_if_exception(ev, "init failed");

	g_print("Resolving service reference from name-service with id \"%s\"\n", id[0]);

	name_service = get_name_service(global_orb, ev);
	abort_if_exception(ev, "failed resolving name service");

	service = (interoperability_Calendar) name_service_resolve(name_service, id, ev);
	abort_if_exception(ev, "failed resolving service at name service");

	client_run(service, ev);
	abort_if_exception(ev, "service not reachable");

	client_cleanup(global_orb, service, ev);
	abort_if_exception(ev, "cleanup failed");

	exit (0);
}
