/*
 * This file was generated by orbit-idl-2 - DO NOT EDIT!
 */

#include <string.h>
#define ORBIT2_STUBS_API
#include "Calendar.h"

interoperability_Appointments* interoperability_Calendar_getAppointments(interoperability_Calendar _obj, const CORBA_char * loginName, const CORBA_char * password, CORBA_Environment *ev){
interoperability_Appointments* _ORBIT_retval;
gpointer _args[2];
_args[0] = (gpointer)&loginName;
_args[1] = (gpointer)&password;
ORBit_c_stub_invoke (_obj, &interoperability_Calendar__iinterface.methods, 0, &_ORBIT_retval, _args, NULL, ev, interoperability_Calendar__classid, G_STRUCT_OFFSET (POA_interoperability_Calendar__epv, getAppointments),
(ORBitSmallSkeleton) _ORBIT_skel_small_interoperability_Calendar_getAppointments);

return _ORBIT_retval;
}
