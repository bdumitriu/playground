package client.interop;

/**
* client/interop/CalendarHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from Calendar.idl
* Sunday, April 10, 2005 10:45:06 PM CEST
*/

public final class CalendarHolder implements org.omg.CORBA.portable.Streamable
{
  public client.interop.Calendar value = null;

  public CalendarHolder ()
  {
  }

  public CalendarHolder (client.interop.Calendar initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = client.interop.CalendarHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    client.interop.CalendarHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return client.interop.CalendarHelper.type ();
  }

}
