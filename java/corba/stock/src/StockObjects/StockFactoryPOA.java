package StockObjects;


/**
* StockObjects/StockFactoryPOA.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from StockObjects.idl
* Wednesday, April 6, 2005 9:18:31 PM CEST
*/

public abstract class StockFactoryPOA extends org.omg.PortableServer.Servant
 implements StockObjects.StockFactoryOperations, org.omg.CORBA.portable.InvokeHandler
{

  // Constructors

  private static java.util.Hashtable _methods = new java.util.Hashtable ();
  static
  {
    _methods.put ("create_stock", new java.lang.Integer (0));
  }

  public org.omg.CORBA.portable.OutputStream _invoke (String $method,
                                org.omg.CORBA.portable.InputStream in,
                                org.omg.CORBA.portable.ResponseHandler $rh)
  {
    org.omg.CORBA.portable.OutputStream out = null;
    java.lang.Integer __method = (java.lang.Integer)_methods.get ($method);
    if (__method == null)
      throw new org.omg.CORBA.BAD_OPERATION (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);

    switch (__method.intValue ())
    {
       case 0:  // StockObjects/StockFactory/create_stock
       {
         String symbol = in.read_string ();
         String description = in.read_string ();
         StockObjects.Stock $result = null;
         $result = this.create_stock (symbol, description);
         out = $rh.createReply();
         StockObjects.StockHelper.write (out, $result);
         break;
       }

       default:
         throw new org.omg.CORBA.BAD_OPERATION (0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    }

    return out;
  } // _invoke

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:StockObjects/StockFactory:1.0"};

  public String[] _all_interfaces (org.omg.PortableServer.POA poa, byte[] objectId)
  {
    return (String[])__ids.clone ();
  }

  public StockFactory _this() 
  {
    return StockFactoryHelper.narrow(
    super._this_object());
  }

  public StockFactory _this(org.omg.CORBA.ORB orb) 
  {
    return StockFactoryHelper.narrow(
    super._this_object(orb));
  }


} // class StockFactoryPOA
