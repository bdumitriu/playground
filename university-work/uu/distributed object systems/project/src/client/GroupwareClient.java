package client;

import core.*;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Observer;

/**
 * A singleton class which provide basic functions for client side.
 * <p/>
 * User: quhw
 * Date: 2005-3-23
 * Time: 15:36:58
 */
public class GroupwareClient
{
    private static GroupwareClient instance = new GroupwareClient();

    private GroupwareManagement management = null;
    private User user = null;

    private int observerId = -1;
    private ClientAppointmentModel appModel = null;

    /**
     * Constructor.
     */
    private GroupwareClient()
    {
    }

    private void CheckUser() throws GroupwareClientException
    {
        if (user == null)
            throw new GroupwareClientException("You have to log in first.");
    }

    /**
     * Get the single instance.
     *
     * @return The single instance of <code>GroupwareClient</code>.
     * @throws GroupwareClientException
     */
    public static GroupwareClient getInstance() throws GroupwareClientException
    {
        if (instance.management == null)
        {
            try
            {
                ServerConfig config = ServerConfig.getInstance();
                String url = "rmi://" + config.getRmiServer() + ":" + config.getRmiPort() + "/groupware";

                instance.management = (GroupwareManagement) Naming.lookup(url);
            }
            catch (Exception e)
            {
                throw new GroupwareClientException(e.getMessage());
            }
        }
        return instance;
    }

    /**
     * Log a user in the server. Only one user can login from a client at a time.
     *
     * @param username The username of the user who want to login.
     * @param password The password of the user.
     * @throws GroupwareClientException
     */
    public void login(String username, String password, ClientAppointmentModel obs) throws GroupwareClientException
    {
        if (user == null)
        {
            try
            {
                user = management.authenticate(username, password);
            }
            catch (Exception e)
            {
                throw new GroupwareClientException(e.getMessage());
            }
        }
        if (user == null)
        {
            throw new GroupwareClientException("Authentication failed.");
        }
        else
        {
            //registerObserver(obs);
            appModel = obs;
        }
    }

    /**
     * Log the user out.
     *
     * @throws GroupwareClientException
     */
    public void logout() throws GroupwareClientException
    {
        if (user != null)
        {
            try
            {
                //unregisterObserver();
                appModel = null;
                management.logOff(user.getAuthToken());
            }
            catch (Exception e)
            {
                throw new GroupwareClientException(e.getMessage());
            }
            user = null;
        }
    }

    /**
     * Get all user's basic data in the system.
     *
     * @return A list of <code>UserData</code>.
     * @throws GroupwareClientException
     */
    public List<UserData> getAllUsers() throws GroupwareClientException
    {
        CheckUser();

        try
        {
            return management.getAllUsers(user.getAuthToken());
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }
	
	public String getLoginName() throws GroupwareClientException
	{
		CheckUser();
		try{return user.getLoginName();}
		catch(Exception e){throw new GroupwareClientException(e.getMessage());}
	}
	
	

    /**
     * Create a new account in the system.
     *
     * @param details  A <code>UserData</code> class which contains all necessory information for creating a new account.
     * @param password The password for the user.
     * @throws GroupwareClientException
     */
    public void createAccount(UserData details, String password, Granularity granularity, String phoneNr, String title)
            throws GroupwareClientException
    {
        try
        {
            management.createAccount(details, password, granularity, phoneNr, title);
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Create a new account
     *
     * @param name     The full username
     * @param login    The login name
     * @param title    The title of the user
     * @param phoneNr  The phone number of the user
     * @param password The password of the user
     * @throws GroupwareClientException
     */
    public void createAccount(String name, String login, String title, String phoneNr, String password) throws GroupwareClientException
    {
        UserData details = new UserData(login, name);
        createAccount(details, password, Granularity.THIRTY_MINUTES, phoneNr, title);
    }

    /**
     * Delete current user account from the system. Current user will be logged out.
     *
     * @throws GroupwareClientException
     */
    public void deleteCurrentAccount() throws GroupwareClientException
    {
        CheckUser();

        try
        {
            management.deleteAccount(user.getAuthToken());
            user = null;
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Get the <code>caldendar</code> of current user.
     *
     * @return
     * @throws GroupwareClientException
     */
    public Calendar getCalendar() throws GroupwareClientException
    {
        CheckUser();
        try
        {
            return user.getCalendar();
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Get current <code>User</code> object.
     *
     * @return The current <code>User</code> object
     * @throws GroupwareClientException
     */
    public User getUser() throws GroupwareClientException
    {
        CheckUser();

        return user;
    }

    /**
     * Check if the user has been logged into the system.
     *
     * @return
     */
    public boolean isLoggedIn()
    {
        if (user != null)
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    /**
     * Set the granularity of the user.
     *
     * @param granularity the granularity: 1 = by the hour, 2 = by half hour, 4 = by 15 minutes
     * @throws GroupwareClientException
     */
    public void setGranularity(int granularity) throws GroupwareClientException
    {
        CheckUser();

        try
        {
            switch (granularity)
            {
                case 1:
                    user.setGranularity(Granularity.ONE_HOUR);
			    break;
                case 2:
                    user.setGranularity(Granularity.THIRTY_MINUTES);
			    break;
                case 4:
                    user.setGranularity(Granularity.FIFTEEN_MINUTES);
			    break;
                default:
                    throw new GroupwareClientException("Unkonwn granularity.");
            }
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Get the granularity of the user
     *
     * @return the granularity: 1 = by the hour, 2 = by half hour, 4 = by 15 minutes
     * @throws GroupwareClientException
     */
    public int getGranularity() throws GroupwareClientException
    {
        CheckUser();

        try
        {
            Granularity granularity = user.getGranularity();

            if (granularity == Granularity.ONE_HOUR)
                return 1;
            else if (granularity == Granularity.THIRTY_MINUTES)
                return 2;
            else if (granularity == Granularity.FIFTEEN_MINUTES)
                return 4;
            else
                throw new GroupwareClientException("Unknown granularity.");
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Determine if there are appointment(s) for the given date.
     *
     * @param year
     * @param month
     * @param day
     * @return
     * @throws GroupwareClientException
     */
    public boolean hasAppointment(int year, int month, int day) throws GroupwareClientException
    {
        Calendar calendar = getCalendar();

        try
        {
            List<Appointment> AllApps = calendar.getAllAppointments();

            for (Appointment app : AllApps)
            {
                Period period = app.getTimeSlot();
                Date date = period.getStartDate();
                java.util.Calendar cal = java.util.Calendar.getInstance();
                cal.setTime(date);
                if (cal.get(java.util.Calendar.YEAR) != year)
                    continue;
                if (cal.get(java.util.Calendar.MONTH) != month)
                    continue;
                if (cal.get(java.util.Calendar.DATE) != day)
                    continue;
                return true;
            }
            return false;
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }	

	public List<Appointment> getAllAppointments() throws GroupwareClientException
	{
		Calendar calendar = getCalendar();
		 
		try
		{
			return calendar.getAllAppointments();
		}
		catch (Exception e)
		{
		    throw new GroupwareClientException(e.getMessage());
		}
	}


    /**
     * Create a single appointment with the given information.
     *
     * @param description
     * @param location
     * @param StartDate
     * @param EndDate
     * @throws GroupwareClientException
     */
    public void createSingleAppointment(String description, String location,
                                        Date StartDate, Date EndDate) throws GroupwareClientException
    {

        Appointment app = new Appointment();
        app.setTimeSlot(new Period(StartDate, EndDate));
        app.setDescription(description);
        app.setLocation(location);

        Calendar calendar = getCalendar();

        try
        {
            if (appModel != null) { appModel.increaseUpdateCount(); }
            calendar.addAppointment(app);
        }
        catch (Exception e)
        {
            if (appModel != null) { appModel.decreaseUpdateCount(); }
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Return a list of time slots which could be used by all of these uesrs.
     * @param users
     * @param duration
     * @param EarliestDate
     * @param LatestDate
     * @return
     * @throws GroupwareClientException
     */ 
    public List<Period> getTimeSlots(List<String> users, int duration, Date EarliestDate, Date LatestDate)
            throws GroupwareClientException
    {
        CheckUser();
        for(String user: users)
        {
            System.out.println(user + duration);
        }
        try
        {
            Group group = management.createGroup(user.getAuthToken(), users);
            return group.getTimeSlots(duration, EarliestDate, LatestDate, user.getGranularity());
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Create a group appointment for a number of users.
     * @param users
     * @param description
     * @param location
     * @param timeslot
     * @throws GroupwareClientException
     */
    public void createGroupAppointment(List<String> users,
                                       String description, String location,
                                       Period timeslot)
            throws GroupwareClientException
    {
        CheckUser();

        try
        {
            Group group = management.createGroup(user.getAuthToken(), users);
            Appointment app = new Appointment();
            app.setTimeSlot(timeslot);
            app.setDescription(description);
            app.setLocation(location);

            if (appModel != null) { appModel.increaseUpdateCount(); }
            group.createGroupAppointment(app);
        }
        catch (Exception e)
        {
            if (appModel != null) { appModel.decreaseUpdateCount(); }
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Get all appointments for the given date.
     * @param year
     * @param month
     * @param date
     * @return
     * @throws GroupwareClientException
     */
    public List<Appointment> getAppointments(int year, int month, int date) throws GroupwareClientException
    {
        Calendar calendar = getCalendar();

        try
        {
            List<Appointment> AllApps = calendar.getAllAppointments();
            List<Appointment> apps = new ArrayList<Appointment>();

            for (Appointment app : AllApps)
            {
                Period period = app.getTimeSlot();
                java.util.Calendar cal = java.util.Calendar.getInstance();
                cal.setTime(period.getStartDate());
                if (cal.get(java.util.Calendar.YEAR) != year)
                    continue;
                if (cal.get(java.util.Calendar.MONTH) != month)
                    continue;
                if (cal.get(java.util.Calendar.DATE) != date)
                    continue;
                apps.add(app);
            }
            return apps;
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Delete an appointment which can be both single appointment and group appointment.
     * @param AppointmentID
     * @throws GroupwareClientException
     */
    public void deleteAppointment(int AppointmentID) throws GroupwareClientException
    {
        CheckUser();

        Calendar calendar = getCalendar();
        try
        {
            if (appModel != null) { appModel.increaseUpdateCount(); }
            calendar.deleteAppointment(AppointmentID);
        }
        catch (Exception e)
        {
            if (appModel != null) { appModel.decreaseUpdateCount(); }
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Delete an group appointment for all users in the group.
     * @param AppointmentID
     * @throws GroupwareClientException
     */
    public void deleteGroupAppointment(int AppointmentID) throws GroupwareClientException
    {
        CheckUser();

        try
        {
            Group group = management.getGroupForAppointment(user.getAuthToken(), AppointmentID);

            if (appModel != null) { appModel.increaseUpdateCount(); }
            group.deleteGroupAppoiment(AppointmentID);
        }
        catch (Exception e)
        {
            if (appModel != null) { appModel.decreaseUpdateCount(); }
            throw new GroupwareClientException(e.getMessage());
        }
    }

    public void changeAppointment(Appointment appointment) throws GroupwareClientException
    {
        CheckUser();

        Calendar calendar = getCalendar();

        try
        {
            if (appModel != null) { appModel.increaseUpdateCount(); }
            calendar.changeAppointment(appointment);
        }
        catch (Exception e)
        {
            if (appModel != null) { appModel.decreaseUpdateCount(); }
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Move an group appointment to a new time slot.
     * @param AppointmentID
     * @param NewTimeSlot
     * @throws GroupwareClientException
     */
    public void moveGroupAppointment(int AppointmentID, Period NewTimeSlot) throws GroupwareClientException
    {
        CheckUser();

        try
        {
            Group group = management.getGroupForAppointment(user.getAuthToken(), AppointmentID);

            if (appModel != null) { appModel.increaseUpdateCount(); }
            group.moveGroupAppointment(AppointmentID, NewTimeSlot);
        }
        catch (Exception e)
        {
            if (appModel != null) { appModel.decreaseUpdateCount(); }
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Get a list of users in the group for an group appointment.
     * @param AppointmentID
     * @return
     * @throws GroupwareClientException
     */
    public List<String> getGroupUsers(int AppointmentID) throws GroupwareClientException
    {
        CheckUser();

        try
        {
            Group group = management.getGroupForAppointment(user.getAuthToken(), AppointmentID);
            return group.getGroupUsers();
        }
        catch (Exception e)
        {
            throw new GroupwareClientException(e.getMessage());
        }
    }

    /**
     * Get the user profile which contains detailed user information for a given username.
     * @param username
     * @return
     * @throws GroupwareClientException
     */
    public UserData getUserProfile(String username) throws GroupwareClientException
    {
        CheckUser();

        List<UserData> all = getAllUsers();
        for(UserData user : all)
        {
            if(user.getLoginName() == username)
                return user;
        }
        throw new GroupwareClientException("The specified user does not exist.");
    }

    private void registerObserver(Observer obs) throws GroupwareClientException
    {
        CheckUser();

        Calendar cal = getCalendar();
        try
	{
            ClientObserverImpl co = new ClientObserverImpl(obs);
            observerId = cal.registerObserver(co);
	}
        catch (Exception e)
	{
            throw new GroupwareClientException(e.getMessage());
	}
    }

    private void unregisterObserver() throws GroupwareClientException
    {
        CheckUser();

        Calendar cal = getCalendar();
        try
	{
            cal.unregisterObserver(observerId);
	}
        catch (Exception e)
	{
            throw new GroupwareClientException(e.getMessage());
	}
    }
}
