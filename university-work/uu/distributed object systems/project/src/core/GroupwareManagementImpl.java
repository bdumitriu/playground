package core;

import core.exceptions.*;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.UID;
import java.rmi.RemoteException;
import java.rmi.NoSuchObjectException;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.util.*;
import java.sql.SQLException;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NameComponent;

/**
 * An implementation of the GroupwareManagement interface.
 *
 * @author Bogdan Dumitriu
 * @author Laurence Cabenda
 * @version 0.1
 * @date Mar 15, 2005
 */
public class GroupwareManagementImpl extends UnicastRemoteObject implements GroupwareManagement
{
	/**
	 * The main method to launch the server.
	 */
	public static void main(String[] args)
	{
		try
		{
			System.out.println("Server initializing...");
			System.out.println("Testing database connection...");
			try
			{
				ConnectionManager.getInstance().getConnection().close();
			}
			catch (SQLException e)
			{}
			System.out.println("Connection to the database is available.");

			int rmiPort = ServerConfig.getInstance().getRmiPort();

			GroupwareManagement gm = GroupwareManagementImpl.getInstance();

			// try to create a new RMI registry on rmiPort and, if this fails, try to find an already
			// running one
			Registry reg = null;
			try
			{
				reg = LocateRegistry.createRegistry(rmiPort);
				System.out.println("Created a RMI registry on port " + rmiPort + ".");
			}
			catch (RemoteException e)
			{
				reg = LocateRegistry.getRegistry(rmiPort);
				System.out.println("A RMI registry is already running on port " + rmiPort +
					", so we use this one.");
			}

			// bind the GroupwareManagement server to the name "groupware" in the newly created RMI server
			reg.rebind("groupware", gm);
			System.out.println("Bound a single remote object in the RMI registry using the name " +
				"\"groupware\".");

			boolean enableInterop = false;
			for (int i = 0; i < args.length; i++)
			{
				if (args[i].equals("--enable-interop"))
				{
					enableInterop = true;
				}
			}

			if (enableInterop)
			{
				initInteroperability(args);
			}

			System.out.println("Server is now ready for service.");
		}
		catch (RemoteException e)
		{
			e.printStackTrace();
			System.out.println("A RemoteException has occured. Server aborting execution.");
			System.exit(1);
		}
	}

	/**
	 * This is the code for the interoperability part of the project.
	 */
	private static void initInteroperability(String[] args)
	{
		try
		{
			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			// get reference to rootpoa & activate the POAManager
			POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			rootpoa.the_POAManager().activate();

			// create servant
			core.interop.CalendarImpl calImpl = new core.interop.CalendarImpl();

			// get object reference from the servant
			org.omg.CORBA.Object ref = rootpoa.servant_to_reference(calImpl);
			core.interop.Calendar href = core.interop.CalendarHelper.narrow(ref);

			// get the root naming context
			org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
			NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
			System.out.println("NamingService IOR: " + orb.object_to_string(objRef));

			// bind the object reference in Naming
			String name = "calendar";
			NameComponent path[] = ncRef.to_name(name);
			ncRef.rebind(path, href);

			System.out.println(orb.object_to_string(ref));
			System.out.println("Interoperability module successfully enabled.");
		}
		catch (Exception e)
		{
			e.printStackTrace();
			System.out.println("An Exception has occured. Server aborting execution.");
			System.exit(1);
		}
	}

	/**
	 * Returns an instance of GroupwareManagementImpl, always the same one.
	 *
	 * @return an instance of GroupwareManagementImpl, always the same one.
	 */
	public static GroupwareManagementImpl getInstance() throws RemoteException
	{
		if (instance == null)
		{
			instance = new GroupwareManagementImpl();
		}

		return instance;
	}

	/**
	 * See the documentation for {@link UserManagement#authenticate(String, String)}.
	 */
	public User authenticate(String userName, String password) throws RemoteException
	{
		System.out.println("User " + userName + " requested authentication.");
		UserImpl user = UserFinder.getInstance().findIfPassword(userName, password);
		if (user != null)
		{
			// if user is already logged in, then log him off first
			String at = checkIfLoggedIn(userName);
			if (at != null)
			{
				try
				{
					System.out.println("User " + userName + " is already logged in. Logging him " +
						"out first.");
					logOff(at);
				}
				catch (AuthenticationFailedException e)
				{}
			}

			String authToken = getAuthToken();
			user.setAuthToken(authToken);
			authenticatedUsers.put(authToken, user);

			System.out.println("Authentication of user " + userName + " succeeded.");
		}
		else
		{
			System.out.println("Authentication of user " + userName + " failed.");
		}

		return user;
	}

	/**
	 * See the documentation for {@link UserManagement#logOff(String)}.
	 */
	public void logOff(String authToken)
		throws RemoteException, AuthenticationFailedException
	{
		UserImpl user;
		if ((user = getAuthUser(authToken)) != null)
		{
			authenticatedUsers.remove(authToken);

			System.out.println("Logging off user " + user.getLoginName() + ".");

			try
			{
				UnicastRemoteObject.unexportObject(user, true);
				UnicastRemoteObject.unexportObject(user.getCalendar(), true);
			}
			catch (NoSuchObjectException e)
			{}

			UserFinder.getInstance().removeFromMap(user.getLoginName());
			CalendarFinder.getInstance().removeFromMap(user.getLoginName());
		}
		else
		{
			System.out.println("Logout request with invalid authToken: \"" + authToken + "\".");
			throw new AuthenticationFailedException();
		}
	}

	/**
	 * See the documentation for {@link UserManagement#getAllUsers(String)}.
	 */
	public List<UserData> getAllUsers(String authToken)
		throws RemoteException, AuthenticationFailedException
	{
		UserImpl user;
		if ((user = getAuthUser(authToken)) != null)
		{
			System.out.println("Supplying user " + user.getLoginName() + " with the list of users in the" +
				" system.");
			return UserFinder.getInstance().findAllUsers();
		}
		else
		{
			System.out.println("Request to get all users with invalid authToken: \"" + authToken + "\".");
			throw new AuthenticationFailedException();
		}
	}

	/**
	 * See the documentation for {@link UserManagement#createAccount(UserData, String, Granularity, String, String)}.
	 */
	public void createAccount(UserData details, String password, Granularity granularity, String phoneNr, String title)
		throws RemoteException, DuplicateLoginNameException
	{
		if (details == null)
		{
			System.out.println("The details object was null in a request to create an account.");
			return;
		}

		System.out.println("Got request to create a new account for user " + details.getLoginName() + ".");
		// create user
		UserImpl user = new UserImpl();

		// set user details
		user.setLoginName(details.getLoginName());
		user.setCalendar(CalendarFinder.getInstance().find(details.getLoginName()));
		try
		{
			user.setName(details.getName(), false);
			user.setPhoneNumber(phoneNr, false);
			user.setTitle(title, false);
		}
		catch (InvalidLoginNameException e)
		{
			// a false in the second position above means we're not writing anything to the database, which
			// means that such an exception will never be thrown
		}

		user.dbInsert(password);

		System.out.println("Successfully createad a new account for user " + details.getLoginName() + ".");
	}

	/**
	 * See the documentation for {@link UserManagement#deleteAccount(String)}.
	 */
	public void deleteAccount(String authToken)
		throws RemoteException, InvalidLoginNameException, AuthenticationFailedException
	{
		UserImpl user;
		if ((user = getAuthUser(authToken)) != null)
		{
			System.out.println("Got request to delete the account of user " + user.getLoginName() + ".");

			logOff(authToken);
			user.dbDelete();

			System.out.println("Successfully deleted the account of user " + user.getLoginName() + ".");
		}
		else
		{
			System.out.println("Request to delete account with invalid authToken: \"" + authToken + "\".");
			throw new AuthenticationFailedException();
		}
	}

	/**
	 * See the documentation for {@link CalendarManagement#createGroup(String, java.util.List<java.lang.String>)}.
	 */
	public Group createGroup(String authToken, List<String> users)
		throws RemoteException, AuthenticationFailedException
	{
		UserImpl user;
		if ((user = getAuthUser(authToken)) != null)
		{
			String curUser = user.getLoginName();
			if (users == null || !users.contains(curUser))
			{
				users.add(curUser);
			}

			System.out.println("Creating a group of " + users.size() + " users at the request of user " +
				curUser + ".");

			return new GroupImpl(users);
		}
		else
		{
			System.out.println("Request to create group with invalid authToken: \"" + authToken + "\".");
			throw new AuthenticationFailedException();
		}
	}

	/**
	 * See the documentation for {@link CalendarManagement#getGroupForAppointment(String, int)}.
	 */
	public Group getGroupForAppointment(String authToken, int appointmentId)
		throws RemoteException, InvalidAppointmentIdException, AuthenticationFailedException
	{
		UserImpl user;
		if ((user = getAuthUser(authToken)) != null)
		{
			try
			{
				GroupImpl groupImpl = (GroupImpl) GroupImpl.getGroupForAppointment(appointmentId);
				if (!groupImpl.getGroupUsers().contains(user.getLoginName()))
				{
					System.out.println("User " + user.getLoginName() + " requested the group " +
						"associated with an appointment and he was not part of the group.");

					throw new InvalidAppointmentIdException();
				}
				else
				{
					System.out.println("Supplying user " + user.getLoginName() + " with the " +
						"group associated with the appointment with id " + appointmentId + ".");

					return groupImpl;
				}
			}
			catch (EmptyGroupException e)
			{
				System.out.println("User " + user.getLoginName() + " requested the group " +
					"associated with an appointment whose id yielded an empty group.");
				throw new InvalidAppointmentIdException();
			}
			catch (NotAGroupAppointmentException e)
			{
				System.out.println("User " + user.getLoginName() + " requested the group " +
					"associated with an appointment which was not a group appointment.");
				throw new InvalidAppointmentIdException();
			}
		}
		else
		{
			System.out.println("Request to get group with invalid authToken: \"" + authToken + "\".");
			throw new AuthenticationFailedException();
		}
	}

		/**
	 * Checks that the <code>authToken</code> is valid and, if so, returns the {@link UserImpl} associated with it.
	 *
	 * @param authToken the authentication token to check.
	 * @return a {@link UserImpl} object if authentication is successful or null if it is not.
	 */
	private UserImpl getAuthUser(String authToken)
	{
		return authenticatedUsers.get(authToken);
	}

	/**
	 * Checks if the user identified by <code>loginName</code> is logged in and, if so, returns the authToken that
	 * is associated with it. Otherwise, it returns null.
	 *
	 * @param loginName the login name of the user you want to check.
	 * @return a valid authToken or null, if the user is not logged in.
	 */
	private String checkIfLoggedIn(String loginName)
	{
		try
		{
			for (Map.Entry<String, UserImpl> entry : authenticatedUsers.entrySet())
			{
				if (loginName.equals(entry.getValue().getLoginName()))
				{
					return entry.getKey();
				}
			}
		}
		catch (RemoteException e)
		{
			// it will never be thrown since this is a local call
		}

		return null;
	}

	/**
	 * Generates a new authentication token and returns it.
	 *
	 * @return a new authentication token.
	 */
	private String getAuthToken()
	{
		UID uid = new UID();
		return uid.toString();
	}

	private GroupwareManagementImpl() throws RemoteException
	{
		super();
	}

	private Map<String, UserImpl> authenticatedUsers = new Hashtable<String, UserImpl>();
	private static GroupwareManagementImpl instance = null;
}
