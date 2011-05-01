package phoneapp;

import java.rmi.*;
import java.rmi.registry.*;
import java.util.Vector;

public class TestClient
{
	public static void main(String args[])
	{
		try
		{
			PhoneBookFactory clientObject = (PhoneBookFactory)
				Naming.lookup("//192.168.0.1:5037/pb-factory");
			PhoneBook pb = clientObject.getPhoneBook("Bogdan");
			System.out.println("Connection established...");
			System.out.println("Starting communication...");
			System.out.println("Creating 1st entry...");
			pb.addEntry(new Entry("Bogdan DUMITRIU", "440058"));
			System.out.println("Creating 2nd entry...");
			pb.addEntry(new Entry("Dana Mircea", "186224"));
			System.out.println("Creating 3rd entry...");
			pb.addEntry(new Entry("Bogdan DUMITRIU", "092.629.000"));
			System.out.print("Looking for Bogdan DUMITRIU... ");
			Vector bog = pb.lookupNumber("Bogdan DUMITRIU");
			if (bog.size() == 0)
			{
				System.out.println("not found");
			}
			else
			{
				System.out.println();
				System.out.println("Bogdan DUMITRIU's nr(s).");
				for (int i = 0; i < bog.size(); i++)
					System.out.println(bog.elementAt(i));
			}
			System.out.print("Looking for Dana Mircea... ");
			Vector dana = pb.lookupNumber("Dana Mircea");
			if (dana.size() == 0)
			{
				System.out.println("not found");
			}
			else
			{
				System.out.println();
				System.out.println("Dana Mircea's nr(s).");
				for (int i = 0; i < dana.size(); i++)
					System.out.println(dana.elementAt(i));
			}
			System.out.print("Looking for somebody else... ");
			Vector smb = pb.lookupNumber("somebody else");
			if (smb.size() == 0)
			{
				System.out.println("not found");
			}
			else
			{
				System.out.println();
				System.out.println("Somebody else's nr(s).");
				for (int i = 0; i < smb.size(); i++)
					System.out.println(smb.elementAt(i));
			}
			System.out.println("Listing all entries...");
			Vector allEntries = pb.list();
			for (int i = 0; i < allEntries.size(); i++)
			{
				System.out.println(allEntries.elementAt(i));
			}
		}
		catch (Exception e)
		{
			System.out.println("TestClient error: " +
				e.getMessage());
			e.printStackTrace();
		}
	}
}
