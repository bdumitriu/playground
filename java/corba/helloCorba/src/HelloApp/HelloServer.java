package HelloApp;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NameComponent;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Feb 26, 2005
 */
public class HelloServer
{
	public static void main(String[] args)
	{
		try
		{
			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			// get reference to rootpoa & activate the POAManager
			POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			rootpoa.the_POAManager().activate();

			// create servant and register it with the ORB
			HelloImpl helloImpl = new HelloImpl();
			helloImpl.setORB(orb);

			// get object reference from the servant
			org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
			Hello href = HelloHelper.narrow(ref);

			// get the root naming context
			org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
			// use NamingContextExt which is part of the Interoperable Naming Service (INS) specification
			NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

			// bind the Object Reference in Naming
			String name = "Hello";
			NameComponent path[] = ncRef.to_name(name);
			ncRef.rebind(path, href);

			System.out.println("HelloServer ready and waiting...");

			// wait for invocations from clients
			orb.run();
		}
		catch (Exception e)
		{
			System.err.println("ERROR: " + e);
			e.printStackTrace(System.out);
		}

		System.out.println("HelloServevr exiting...");
	}
}

class HelloImpl extends HelloPOA
{
	public void setORB(ORB orb)
	{
		this.orb = orb;
	}

	public String sayHello()
	{
		return "\nHello world!\n";
	}

	public void shutdown()
	{
		orb.shutdown(false);
	}

	private ORB orb;
}
