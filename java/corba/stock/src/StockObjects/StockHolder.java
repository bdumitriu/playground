package StockObjects;

/**
* StockObjects/StockHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from StockObjects.idl
* Wednesday, April 6, 2005 9:18:31 PM CEST
*/

public final class StockHolder implements org.omg.CORBA.portable.Streamable
{
  public StockObjects.Stock value = null;

  public StockHolder ()
  {
  }

  public StockHolder (StockObjects.Stock initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = StockObjects.StockHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    StockObjects.StockHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return StockObjects.StockHelper.type ();
  }

}