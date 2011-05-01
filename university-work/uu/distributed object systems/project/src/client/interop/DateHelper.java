package client.interop;


/**
* client/interop/DateHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from Calendar.idl
* Sunday, April 10, 2005 10:45:06 PM CEST
*/

abstract public class DateHelper
{
  private static String  _id = "IDL:interoperability/Date:1.0";

  public static void insert (org.omg.CORBA.Any a, client.interop.Date that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static client.interop.Date extract (org.omg.CORBA.Any a)
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
            "second",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[1] = new org.omg.CORBA.StructMember (
            "minute",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[2] = new org.omg.CORBA.StructMember (
            "hour",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[3] = new org.omg.CORBA.StructMember (
            "day",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[4] = new org.omg.CORBA.StructMember (
            "month",
            _tcOf_members0,
            null);
          _tcOf_members0 = org.omg.CORBA.ORB.init ().get_primitive_tc (org.omg.CORBA.TCKind.tk_long);
          _members0[5] = new org.omg.CORBA.StructMember (
            "year",
            _tcOf_members0,
            null);
          __typeCode = org.omg.CORBA.ORB.init ().create_struct_tc (client.interop.DateHelper.id (), "Date", _members0);
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

  public static client.interop.Date read (org.omg.CORBA.portable.InputStream istream)
  {
    client.interop.Date value = new client.interop.Date ();
    value.second = istream.read_long ();
    value.minute = istream.read_long ();
    value.hour = istream.read_long ();
    value.day = istream.read_long ();
    value.month = istream.read_long ();
    value.year = istream.read_long ();
    return value;
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, client.interop.Date value)
  {
    ostream.write_long (value.second);
    ostream.write_long (value.minute);
    ostream.write_long (value.hour);
    ostream.write_long (value.day);
    ostream.write_long (value.month);
    ostream.write_long (value.year);
  }

}
