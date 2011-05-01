package HelloApp;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextExt;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Feb 26, 2005
 */
public class HelloClient
{
	public static void main(String[] args)
	{
		try
		{
			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			// get the root naming context
			org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
			// use NamingContextExt instead of NamingContext: this is part of the Interoperable Naming Service.
			NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

			// resolve the object reference in Naming
			String name = "Hello";
			helloImpl = HelloHelper.narrow(ncRef.resolve_str(name));

			System.out.println("Obtained a handle on server object: " + helloImpl);
			System.out.println(helloImpl.sayHello());
			helloImpl.shutdown();
		}
		catch (Exception e)
		{
			System.out.println("ERROR: " + e) ;
			e.printStackTrace(System.out);
		}
	}

	static Hello helloImpl;
}
