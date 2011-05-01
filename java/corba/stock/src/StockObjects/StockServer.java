package StockObjects;

import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.FileWriter;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 6, 2005
 */
public class StockServer
{
	public static void main(String[] args)
	{
		try
		{
			// Initialize the ORB
			org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init(args, null);

			// Get reference to RootPOA & activate the POAManager
			org.omg.PortableServer.POA rootpoa =
				org.omg.PortableServer.POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			rootpoa.the_POAManager().activate();

			// Create a stock object
			StockImpl theStock = new StockImpl("TLV", "Banca Transilvania");

			// Get object reference from the servant
			org.omg.CORBA.Object ref = rootpoa.servant_to_reference(theStock);
			Stock stockRef = StockHelper.narrow(ref);

			// Get the root naming context
			//org.omg.CosNaming.NamingContextExt ncRef =
			//	org.omg.CosNaming.NamingContextExtHelper.narrow(orb.resolve_initial_references("NameService"));

			// Bind the Object Reference in the naming context
			//org.omg.CosNaming.NameComponent path[] = ncRef.to_name("Stock");
			//ncRef.rebind(path, stockRef);

			// Write stringified object reference to a file
			PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(args[0])));
			out.println(orb.object_to_string(stockRef));
			out.close();

			//System.out.println("The object_to_string method returned: " + orb.object_to_string(stockRef));

			System.out.println("Stock server ready and waiting...");

			// wait for invocations from clients
			orb.run();
		}
		catch (Exception e)
		{
			System.err.println("Stock server error: " + e);
			e.printStackTrace(System.out);
		}
	}
}
