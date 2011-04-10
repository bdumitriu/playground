package client.interop;

/**
* client/interop/DateHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from Calendar.idl
* Sunday, April 10, 2005 10:45:06 PM CEST
*/

public final class DateHolder implements org.omg.CORBA.portable.Streamable
{
  public client.interop.Date value = null;

  public DateHolder ()
  {
  }

  public DateHolder (client.interop.Date initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = client.interop.DateHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    client.interop.DateHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return client.interop.DateHelper.type ();
  }

}