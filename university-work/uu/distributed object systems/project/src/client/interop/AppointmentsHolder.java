package client.interop;


/**
* client/interop/AppointmentsHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from Calendar.idl
* Sunday, April 10, 2005 10:45:06 PM CEST
*/

public final class AppointmentsHolder implements org.omg.CORBA.portable.Streamable
{
  public client.interop.Appointment value[] = null;

  public AppointmentsHolder ()
  {
  }

  public AppointmentsHolder (client.interop.Appointment[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = client.interop.AppointmentsHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    client.interop.AppointmentsHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return client.interop.AppointmentsHelper.type ();
  }

}