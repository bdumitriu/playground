package core.interop;


/**
* core/interop/Date.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from Calendar.idl
* Sunday, April 10, 2005 10:19:45 PM CEST
*/

public final class Date implements org.omg.CORBA.portable.IDLEntity
{
  public int second = (int)0;
  public int minute = (int)0;
  public int hour = (int)0;
  public int day = (int)0;
  public int month = (int)0;
  public int year = (int)0;

  public Date ()
  {
  } // ctor

  public Date (int _second, int _minute, int _hour, int _day, int _month, int _year)
  {
    second = _second;
    minute = _minute;
    hour = _hour;
    day = _day;
    month = _month;
    year = _year;
  } // ctor

} // class Date
