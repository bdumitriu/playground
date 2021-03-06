package client.interop;


/**
* client/interop/AppointmentHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from Calendar.idl
* Sunday, April 10, 2005 10:45:06 PM CEST
*/

abstract public class AppointmentHelper
{
  private static String  _id = "IDL:interoperability/Appointment:1.0";

  public static void insert (org.omg.CORBA.Any a, client.interop.Appointment that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static client.interop.Appointment extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  private static boolean __active = false;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      synchronized (org.omg.CORBA.TypeCode.class)
      {
        if (__typeCode == null)
        {
          if (__active)
          {
            return org.omg.CORBA.ORB.init().create_recursive_tc ( _id );
          }
          __active = true;
          org.omg.CORBA.StructMember[] _members0 = new org.omg.CORBA.StructMember [6];
          org.omg.CORBA.TypeCode _tcOf_members0 = null;
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[0] = new org.omg.CORBA.StructMember (
            "id",
            _tcOf_members0,
            null);
          _tcOf_members0 = client.interop.DateHelper.type ();
          _members0[1] = new org.omg.CORBA.StructMember (
            "startDate",
            _tcOf_members0,
            null);
          _tcOf_members0 = client.interop.DateHelper.type ();
          _members0[2] = new org.omg.CORBA.StructMember (
            "endDate",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().create_string_tc (0);
          _members0[3] = new org.omg.CORBA.StructMember (
            "location",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().create_string_tc (0);
          _members0[4] = new org.omg.CORBA.StructMember (
            "description",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_boolean);
          _members0[5] = new org.omg.CORBA.StructMember (
            "isGroupAppointment",
            _tcOf_members0,
            null);
          __typeCode = org.omg.CORBA.ORB.init ().create_struct_tc (client.interop.AppointmentHelper.id (), "Appointment", _members0);
          __active = false;
        }
      }
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static client.interop.Appointment read (org.omg.CORBA.portable.InputStream istream)
  {
    client.interop.Appointment value = new client.interop.Appointment ();
    value.id = istream.read_long ();
    value.startDate = client.interop.DateHelper.read (istream);
    value.endDate = client.interop.DateHelper.read (istream);
    value.location = istream.read_string ();
    value.description = istream.read_string ();
    value.isGroupAppointment = istream.read_boolean ();
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, client.interop.Appointment value)
  {
    ostream.write_long (value.id);
    client.interop.DateHelper.write (ostream, value.startDate);
    client.interop.DateHelper.write (ostream, value.endDate);
    ostream.write_string (value.location);
    ostream.write_string (value.description);
    ostream.write_boolean (value.isGroupAppointment);
  }

}
