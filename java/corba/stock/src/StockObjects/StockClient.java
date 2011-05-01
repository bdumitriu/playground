package StockObjects;

import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextExt;

import java.io.FileReader;
import java.io.BufferedReader;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 6, 2005
 */
public class StockClient
{
	public static void main(String[] args)
	{
		try
		{
			// Create and initialize the ORB
			org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init(args, null);

			// Get the root naming context
			//org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
			// use NamingContextExt instead of NamingContext: this is part of the Interoperable Naming Service.
			//NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

			// resolve the object reference in Naming
			//String name = "Stock";
			//stockImpl = StockHelper.narrow(ncRef.resolve_str(name));

			// Write stringified object reference to a file
			BufferedReader in = new BufferedReader(new FileReader(args[0]));
			String objName = in.readLine();
			in.close();

			org.omg.CORBA.Object obj = orb.string_to_object(objName);
			stockImpl = StockHelper.narrow(obj);

			//System.out.println("Obtained a handle on server object: " + stockImpl);

			System.out.println("Obtained stock object for company: " + stockImpl.description());
		}
		catch (Exception e)
		{
			System.out.println("Stock client error: " + e) ;
			e.printStackTrace(System.out);
		}
	}

	static Stock stockImpl;
}
