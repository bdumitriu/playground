package client;

import core.Granularity;
import core.Period;
import core.UserData;
import core.User;
import core.Appointment;
import core.exceptions.InvalidLoginNameException;
import java.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Observable;
import java.util.Vector;
/**
 * 
 * This class represent the GUI of the calendar
 *
 * @author Richard Nieuwenhuis
 * @version 0.1
 * @date Apr 11, 2005
 */
public class ClientController implements Observer
{
	private ClientModel model;
	private ClientAppointmentModel appModel;
	private JMenuBar menuBar;
	private JFrame frame;
	private JPanel calendarPane, dayPane, datePane, mainPane, appointmentPane, centerPane, southPane;
	private JComboBox monthBox, yearBox, bTimeBox, userBox, edit, delete;
	private JTextField location, loginName, loginC, phone, name, title, durMin;
	private JPasswordField password, p1, p2;
	private JSpinner eDate, lDate, bTime, eTime;
	private JTextArea description, selUsersT;
	private GroupwareClient client;
	private ClientFieldChecker checker;
	private List<UserData> users, selUsers;
	private Vector v;

	public ClientController()
	{
		try
		{
			client = GroupwareClient.getInstance();
		}
		catch (Exception e)
		{
			System.out.println("Failed to initialize RMI communication: " + e.getMessage());
			System.exit(1);
		}
		model    = new ClientModel();
		appModel = new ClientAppointmentModel(this);
		checker = new ClientFieldChecker();
		JFrame.setDefaultLookAndFeelDecorated(true);
		frame = new JFrame();
		frame.setTitle("Calendar");		
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent event)
			{
				try
				{
					client.logout();
					System.exit(0);
				}
				catch (Exception e)
				{}
			}
		});	
		menuBar = new JMenuBar();
		mainPane = new JPanel();
		mainPane.setLayout(new BorderLayout());
		createLoginScreen();
		frame.setContentPane(mainPane);
		frame.setVisible(true);
		frame.pack();
		frame.setLocationRelativeTo(null);
	}
	
	/**
	 * Shows the login screen	
	 */

	private void createLoginScreen()
	{
		JPanel loginPanel = new JPanel();
		JButton submit = new JButton("Login");
		JButton create = new JButton("Create Account");

		submit.addActionListener(new LoginListener());
		create.addActionListener(new CreateListener());

		loginPanel.setLayout(new BorderLayout());

		loginName = new JTextField(10);
		password = new JPasswordField(10);

		JPanel centerPanel = new JPanel(new FlowLayout());
		JPanel southPanel = new JPanel(new FlowLayout());

		JLabel l1 = new JLabel("Login name:");
		l1.setLabelFor(loginName);
		JLabel l2 = new JLabel("Password:");
		l1.setLabelFor(password);

		centerPanel.add(l1);
		centerPanel.add(loginName);
		centerPanel.add(l2);
		centerPanel.add(password);
		centerPanel.add(submit);
		southPanel.add(create, BorderLayout.SOUTH);
		loginPanel.add(centerPanel, BorderLayout.CENTER);
		loginPanel.add(southPanel, BorderLayout.SOUTH);

		mainPane.add(loginPanel);
		mainPane.setOpaque(true);
	}

	
	/**
	 * Shows the screen for creating an account	
	 */
	private void createAccountScreen()
	{
		JPanel createPanel = new JPanel();
		createPanel.setLayout(new BorderLayout());
		JButton submit = new JButton("Submit");

		JPanel centerPanel = new JPanel(new GridLayout(6, 2));
		JPanel southPanel = new JPanel(new FlowLayout());

		name = new JTextField(10);

		loginC = new JTextField(10);
		phone = new JTextField(10);
		title = new JTextField(10);
		p1 = new JPasswordField(10);
		p2 = new JPasswordField(10);
		JLabel l1 = new JLabel("Name: ");
		l1.setLabelFor(name);
		JLabel l2 = new JLabel("Login name: ");
		l2.setLabelFor(loginC);
		JLabel l3 = new JLabel("Phone number: ");
		l3.setLabelFor(phone);
		JLabel l4 = new JLabel("Title: ");
		l4.setLabelFor(title);
		JLabel l5 = new JLabel("Password: ");
		l5.setLabelFor(p1);
		JLabel l6 = new JLabel("Password Again: ");
		l6.setLabelFor(p2);
		submit.addActionListener(new CreateAccountListener());

		centerPanel.add(l1);
		centerPanel.add(name);
		centerPanel.add(l2);
		centerPanel.add(loginC);
		centerPanel.add(l3);
		centerPanel.add(phone);
		centerPanel.add(l4);
		centerPanel.add(title);
		centerPanel.add(l5);
		centerPanel.add(p1);
		centerPanel.add(l6);
		centerPanel.add(p2);

		southPanel.add(submit);

		createPanel.add(centerPanel, BorderLayout.CENTER);
		createPanel.add(southPanel, BorderLayout.SOUTH);

		mainPane.add(createPanel);
		mainPane.setOpaque(true);
	}

	
	/**
	 * This creates the main screen eg. celendar, menubar	
	 */
	private void createMainScreen()
	{
		System.out.println("Main screenen");
		try{appModel.setAppointments(client.getAllAppointments());}
		catch(GroupwareClientException ex){showErrorMessage("Error getting the appointments");}
		centerPane = new JPanel(new FlowLayout());
		southPane = new JPanel(new BorderLayout());
		createMenuBar();
		calendarPane = new JPanel(new GridLayout(7, 7));
		createCalendar();
		createDateComboBox();
		appointmentPane = new JPanel();
		appointmentPane.setLayout(new BorderLayout());
		dayPane = new JPanel();
		dayPane.setLayout(new BorderLayout());
		frame.setJMenuBar(menuBar);
		centerPane.add(calendarPane);
		centerPane.add(datePane);
		southPane.add(appointmentPane, BorderLayout.CENTER);
		southPane.add(dayPane, BorderLayout.SOUTH);
		mainPane.add(centerPane, BorderLayout.CENTER);
		mainPane.add(southPane, BorderLayout.SOUTH);
		mainPane.setOpaque(true);
	}

	private void createCalendar()
	{
		calendarPane.removeAll();
		createCalendarHeader(calendarPane);
		createRows(calendarPane);
	}

	private void refreshScreen()
	{
		frame.pack();
		//frame.setLocationRelativeTo(null);
	}

	private void emptyScreen()
	{		
		mainPane.removeAll();
	}

	/**
	 * After a new date has been set, the calendar pane
	 * has to be updated, this method will create all the day buttons
	 * 
	 * @param panel the panel where all the buttons must be put in
	 */
	private void createRows(JPanel panel)
	{
		List<Appointment> apps;
		Font f = new Font("Arial", Font.ITALIC, 13);
		JButton b;

		int firstDay = model.getFirstDayOfWeek();
		for (int i = 1; i < firstDay; i++)
		{
			panel.add(new JPanel());
		}

		int dayOfMonth = model.getDaysOfMonth();
		for (int i = 1; i <= dayOfMonth; i++)
		{
			b = new JButton(i + "");
			try
			{
				apps = appModel.getAppointments(model.getYear(), model.getMonth(), i);
				if (!apps.isEmpty())
				{					
					b.setFont(f);
				}
			}
			catch(Exception ex)
			{
				showErrorMessage("Error getting the appointments");
				apps = new ArrayList<Appointment>();
			}
			b.setMargin(new Insets(0, 0, 0, 0));
			b.addActionListener(new DayButtonListener(i, apps));
			panel.add(b);
		}

		for (int i = panel.getComponentCount(); i < 49; i++)
		{
			panel.add(new JPanel());
		}
	}

	private void createCalendarHeader(JPanel panel)
	{
		String[] days = {"M", "T", "W", "T", "F", "S", "S"};
		JButton b;
		for (int i = 0; i < days.length; i++)
		{
			b = new JButton(days[i]);
			b.setMargin(new Insets(0, 0, 0, 0));
			b.setEnabled(false);
			panel.add(b);
		}
	}
	
	/**
	 * This method will display the two combo boxes for seleting
	 * the month and the year the user wants to be displayed	
	 */

	private void createDateComboBox()
	{
		datePane = new JPanel(new FlowLayout());

		String[] months = {"January", "February", "March", "April",
				   "May", "June", "July", "August", "September",
				   "October", "November", "December"};
		int year = model.getCurrentYear();
		int offset = 10;
		monthBox = new JComboBox(months);
		monthBox.setSelectedIndex(model.getMonth());
		monthBox.addActionListener(new DateComboBoxListener());

		yearBox = new JComboBox();
		for (int i = (year - offset); i < (year + offset); i++)
		{
			yearBox.addItem(i + "");
		}
		yearBox.setSelectedIndex(offset);
		yearBox.addActionListener(new DateComboBoxListener());
		JLabel l1 = new JLabel("Month: ");
		l1.setLabelFor(monthBox);
		l1.setMinimumSize(monthBox.getPreferredSize());
		JLabel l2 = new JLabel("Year: ");
		l2.setLabelFor(yearBox);
		l2.setMinimumSize(yearBox.getPreferredSize());
		datePane.add(l1);
		datePane.add(monthBox);
		datePane.add(l2);
		datePane.add(yearBox);
	}

	/**
	 * This will show the form for creating or modifying a single
	 * appointment.
	 * 
	 * @param app the appointment being edited or not
	 */
	
	private void createSingleAppointment(Appointment app)
	{
		appointmentPane.removeAll();
		String des = "";
		String loc = "";
		JPanel centerPanel = new JPanel(new FlowLayout());
		JPanel southPanel = new JPanel(new FlowLayout());
		int granularity = model.getGranularityInTime();
		if(app!=null)
		{
			
			des = app.getDescription();
			loc = app.getLocation();
			Date startTime = app.getTimeSlot().getStartDate();
			monthBox.setSelectedIndex(model.getMonth());
			yearBox.setSelectedItem(model.getYear()+"");
			
			model.setToDate(startTime, "<bij editen zet ik hem hiernaar");
			//createCalendar();
			
			Date endTime   = app.getTimeSlot().getEndDate();
			bTime = new JSpinner(new GranularitySpinnerDateModel(granularity, startTime));
			eTime = new JSpinner(new GranularitySpinnerDateModel(granularity, endTime));			
		}
		else
		{
			bTime = new JSpinner(new GranularitySpinnerDateModel(granularity));
			eTime = new JSpinner(new GranularitySpinnerDateModel(granularity));
		}

		JButton submit;			
		
		bTime.setEditor(new JSpinner.DateEditor(bTime, "KK:mm a"));
		JLabel l1 = new JLabel("Begin time: ");
		l1.setLabelFor(bTime);
		l1.setMinimumSize(bTime.getPreferredSize());

		
		eTime.setEditor(new JSpinner.DateEditor(eTime, "KK:mm a"));
		JLabel l2 = new JLabel("End time: ");
		l2.setLabelFor(eTime);
		l2.setMinimumSize(eTime.getPreferredSize());
		location = new JTextField(loc, 10);
		JLabel l3 = new JLabel("Location: ");
		l3.setLabelFor(location);
		l3.setMinimumSize(location.getPreferredSize());
		description = new JTextArea(des, 5, 10);
		JLabel l4 = new JLabel("Description: ");
		l4.setLabelFor(description);
		l4.setMinimumSize(description.getPreferredSize());
		submit = new JButton("Submit");
		submit.setMargin(new Insets(0, 0, 0, 0));
		submit.addActionListener(new SingleAppointmentListener(app));

		centerPanel.add(l1);
		centerPanel.add(bTime);
		centerPanel.add(l2);
		centerPanel.add(eTime);
		centerPanel.add(l3);
		centerPanel.add(location);
		southPanel.add(l4);
		southPanel.add(description);

		southPanel.add(submit);
		appointmentPane.add(centerPanel, BorderLayout.CENTER);
		appointmentPane.add(southPanel, BorderLayout.SOUTH);
		
	}

	/**
	 * Show an error message dialog
	 * 
	 * @param message the error message
	 */
	
	private void showErrorMessage(String message)
	{
		JOptionPane.showMessageDialog(frame, message, "An Error has occured",
			JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * Show an information method. For example if an appointment
	 * has successfully been added
	 * 
	 * @param message the message
	 * @param title the title of the information message
	 */
	
	private void showInfoMessage(String message, String title)
	{
		JOptionPane.showMessageDialog(frame, message, title,
			JOptionPane.INFORMATION_MESSAGE);
	}
	
	/**
	 * Shows the form for creating or editing a group appointment.
	 * 
	 * @param app the appointment being edited or not
	 */

	private void createGroupAppointment(Appointment app)
	{
		appointmentPane.removeAll();
		String des = "";
		String loc = "";
		String un = "";
		int id    = -1;
		JLabel l1 = new JLabel();
		int granularity = model.getGranularityInTime();	
			
		selUsers = new ArrayList<UserData>();
		v = new Vector();
		
		JPanel centerPanel = new JPanel(new BorderLayout());
		JPanel southPanel = new JPanel(new BorderLayout());
		JPanel centerUpperPanel = new JPanel(new GridLayout(3, 2));
		JPanel centerLowerPanel = new JPanel(new GridLayout(2, 2));
		JPanel southUpperPanel = new JPanel(new FlowLayout());
		JPanel southLowerPanel = new JPanel(new FlowLayout());

		if(app!=null)
		{
			Date startDate = app.getTimeSlot().getStartDate();
			Date endDate   = app.getTimeSlot().getEndDate();
			des            = app.getDescription();
			loc            = app.getLocation();
			id             = app.getId();
			eDate = new JSpinner(new GranularitySpinnerDateModel(granularity, startDate));
			lDate = new JSpinner(new GranularitySpinnerDateModel(granularity, endDate));
			v.add("x"); v.add("y");
		}
		
		else
		{
			try
			{				
				users = client.getAllUsers();				
				un    = client.getLoginName();				
			}		
			catch (GroupwareClientException ex){showErrorMessage(ex.getMessage());}
			eDate = new JSpinner(new GranularitySpinnerDateModel(granularity));
			lDate = new JSpinner(new GranularitySpinnerDateModel(granularity));
			v.add(un);
			userBox = new JComboBox();
			userBox.addActionListener(new UserComboBoxListener());
			int i = 0; int j = 0;
			for (UserData userData : users)
			{
			        if(userData.getLoginName().equals(un))
						j = i;			
			        else
						userBox.addItem(userData.getName());					
					i++;
			}
		    users.remove(j);
			l1 = new JLabel("Add/Remove User: ");
			l1.setLabelFor(userBox);
			l1.setMinimumSize(userBox.getPreferredSize());
		}

		durMin = new JTextField(5);
		JLabel l4 = new JLabel("Duration in mins: ");
		l4.setLabelFor(durMin);
		l4.setMinimumSize(durMin.getPreferredSize());
		location = new JTextField(loc, 5);
		JLabel l5 = new JLabel("Location: ");
		l5.setLabelFor(location);
		l5.setMinimumSize(location.getPreferredSize());
		if(app!=null){centerUpperPanel.add(new JPanel()); centerUpperPanel.add(new JPanel());}
		else { centerUpperPanel.add(l1); centerUpperPanel.add(userBox);}
		centerUpperPanel.add(l4);
		centerUpperPanel.add(durMin);
		centerUpperPanel.add(l5);
		centerUpperPanel.add(location);

		
		JLabel l7 = new JLabel("Earliest date: ");
		l7.setLabelFor(eDate);
		l7.setMinimumSize(eDate.getPreferredSize());

		JLabel l8 = new JLabel("Latest date: ");
		l8.setLabelFor(lDate);
		l8.setMinimumSize(lDate.getPreferredSize());

		centerLowerPanel.add(l7);
		centerLowerPanel.add(eDate);

		centerLowerPanel.add(l8);
		centerLowerPanel.add(lDate);

		centerPanel.add(centerUpperPanel, BorderLayout.CENTER);
		centerPanel.add(centerLowerPanel, BorderLayout.SOUTH);
		JButton next = new JButton("Next");
		next.addActionListener(new NextListener(app));
		JButton submit = new JButton("Submit");
		submit.addActionListener(new SubmitGroupChangeListener(app));
		description = new JTextArea(des, 5, 10);
		JLabel l6 = new JLabel("Description: ");
		l6.setLabelFor(description);
		l6.setMinimumSize(description.getPreferredSize());
		
		southUpperPanel.add(l6);
		southUpperPanel.add(description);
		
		selUsersT = new JTextArea(un+"\n", 5, 10);
		JLabel l9 = new JLabel("Selected users: ");
		l9.setLabelFor(description);
		l9.setMinimumSize(description.getPreferredSize());
		JScrollPane scrollPane = new JScrollPane(selUsersT,
			JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		selUsersT.setEditable(false);

		if(app==null){ southUpperPanel.add(l9);	southUpperPanel.add(scrollPane);}
		if(app!=null){southLowerPanel.add(submit);}
		southLowerPanel.add(next);
		southPanel.add(southUpperPanel, BorderLayout.CENTER);
		southPanel.add(southLowerPanel, BorderLayout.SOUTH);
		appointmentPane.add(centerPanel, BorderLayout.CENTER);
		appointmentPane.add(southPanel, BorderLayout.SOUTH);
	}

	/**
	 * Shows the form for seleting a timeslot when creating or
	 * editing a group appointment
	 * 
	 * @param id the id of the appointment being updated or not
	 * @param periods a list of all the available timeslots
	 * @param usernames a list of all the usernames that are participating in a group appointment
	 * @param loc the location of the group appointment
	 * @param des the description of the group appointment
	 */
	
	private void showTimeSlots(int id, List<Period> periods, List<String> usernames, String loc, String des)
	{
		appointmentPane.removeAll();		
		JButton submit = new JButton("Submit");

		String timeslot = "";
		bTimeBox = new JComboBox();
		JLabel l1 = new JLabel("Select time slot: ");
		l1.setLabelFor(bTimeBox);
		l1.setMinimumSize(bTimeBox.getPreferredSize());
		Date oldDate = model.getDate();

		for (Period period : periods)
		{
			timeslot = "";
			model.setToDate(period.getStartDate());
			timeslot = model.getDateRep();
			model.setToDate(period.getEndDate());
			timeslot += " to " + model.getDateRep();
			bTimeBox.addItem(timeslot);
		}
		
		model.setToDate(oldDate);

		submit.addActionListener(new TimeSlotListener(id, periods, usernames, des, loc));
		JPanel centerPanel = new JPanel(new FlowLayout());
		JPanel southPanel = new JPanel(new FlowLayout());
		centerPanel.add(l1);
		centerPanel.add(bTimeBox);
		southPanel.add(submit);
		appointmentPane.add(centerPanel, BorderLayout.CENTER);
		appointmentPane.add(southPanel, BorderLayout.SOUTH);

	}
	
	
	/**
	 * Its possible the user only wants to edit the description or
	 * location of the group appointment. So this method will only update
	 * these two fields
	 * 
	 * @param app the appointment being edited	
	 */
	private void changeGroupAppointment(Appointment app)
	{
		app.setLocation(location.getText());
		app.setDescription(description.getText());
		app.setTimeSlot(app.getTimeSlot());
		app.setGroupAppointment(true);
		try{client.changeAppointment(app);}
		catch(Exception ex){showErrorMessage("Error updating appointment");}
	}

	/**
	 * User can select or deselect people for a group appointment. This method
	 * will remove or add a user to the selected users list
	 * 
	 * @param index the index of the user in the user list being added or deleted
	 */
	private void addOrDelToDelBox(int index)
	{
		UserData user = users.get(index);
		if (selUsers.contains(user))
		{
			selUsers.remove(user);
			v.remove(user.getName());
			selUsersT.setText("");
			for (int j = 0; j < v.size(); j++)
			{
				selUsersT.append((String) v.elementAt(j) + "\n");
			}
		}
		else
		{
			selUsers.add(users.get(index));
			selUsersT.append(user.getName() + "\n");
			v.add(user.getName());
		}
	}

	
	/**
	 * Shows a JTable containg the appointments of the day the user has selected	
	 */
	private void showDay(List<Appointment> apps)
	{
		dayPane.removeAll();
		JPanel centerPanel = new JPanel(new BorderLayout());
		JPanel southPanel = new JPanel(new FlowLayout());
		TableSorter sorter = new TableSorter(new DayTable(apps, model));
		JTable table = new JTable(sorter);
		sorter.setTableHeader(table.getTableHeader());
		table.setPreferredScrollableViewportSize(new Dimension(700, 100));
		table.getTableHeader().setToolTipText("Click to specify sorting; Control-Click to specify secondary sorting");
		JScrollPane scrollPane = new JScrollPane(table,
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		edit = new JComboBox();
		delete = new JComboBox();
		delete.addActionListener(new DeleteComboBoxListener(apps));
		edit.addActionListener(new EditComboBoxListener(apps));
		for(int i = 0; i<apps.size(); i++)
		{
			edit.addItem(""+i);
			delete.addItem(""+i);
		}
		JLabel l1 = new JLabel("Edit: ");
		l1.setLabelFor(edit);
		l1.setMinimumSize(edit.getPreferredSize());
		JLabel l2 = new JLabel("Delete: ");
		l2.setLabelFor(delete);
		l2.setMinimumSize(delete.getPreferredSize());
		centerPanel.add(scrollPane);
		southPanel.add(l1);
		southPanel.add(edit);
		southPanel.add(l2);
		southPanel.add(delete);
		dayPane.add(centerPanel, BorderLayout.CENTER);
		dayPane.add(southPanel, BorderLayout.SOUTH);
	}

	private void createMenuBar()
	{
		JMenu menu;
		JMenuItem menuItem;

		menu = new JMenu("File");
		menu.setMnemonic(KeyEvent.VK_F);
		menuBar.add(menu);
		menuItem = new JMenuItem("Close");
		menuItem.addActionListener(new ItemCloseListener());
		menu.add(menuItem);
		menuItem = new JMenuItem("Logout");
		menuItem.addActionListener(new ItemLogoutListener());
		menu.add(menuItem);
		menuItem = new JMenuItem("Delete Account");
		menuItem.addActionListener(new ItemDeleteListener());
		menu.add(menuItem);
		menu = new JMenu("Create");
		menu.setMnemonic(KeyEvent.VK_C);
		menuBar.add(menu);
		menuItem = new JMenuItem("Single appointment");
		menuItem.addActionListener(new ItemSingleAppointmentListener());
		menu.add(menuItem);
		menuItem = new JMenuItem("Group appointment");
		menuItem.addActionListener(new ItemGroupAppointmentListener());
		menu.add(menuItem);
		menu = new JMenu("Granularity");
		menu.setMnemonic(KeyEvent.VK_M);
		menuBar.add(menu);
		menuItem = new JMenuItem("By the quarter of hour");
		menuItem.addActionListener(new ItemGranularityListener(4));
		menu.add(menuItem);
		menuItem = new JMenuItem("By half hour");
		menuItem.addActionListener(new ItemGranularityListener(2));
		menu.add(menuItem);
		menuItem = new JMenuItem("By the hour");
		menuItem.addActionListener(new ItemGranularityListener(1));
		menu.add(menuItem);
	}
	
	public void update(Observable observable, Object o)
	{

		int data = ((Integer) o).intValue();

		// this is the id of the appointment which got inserted/updated/deleted
		int changedAppId = (int) data / 10;

		// this is a code for the operation that was done with the appointment
		//  opCode = 0: app. was inserted
		//  opCode = 1: app. was updated
		//  opCode = 2: app. was deleted
		int opCode = data % 10;

		Appointment app = appModel.getNewAppointment(changedAppId);
		if (app != null)
		{
			if (opCode == 0)
			{
				showInfoMessage("New group appointment added", "New Appointment");
			}
			else if (opCode == 1)
			{
				showInfoMessage("Group appointment changed.", "Appointment Changed");
			}

			model.setToDate(app.getTimeSlot().getStartDate());
			monthBox.setSelectedIndex(model.getMonth());
			yearBox.setSelectedItem(model.getYear()+"");

			// I call it again because in the selection listener for monthBox/yearBox you call
			// setDate(month, year), which resets the DAY_OF_MONTH to 1
			model.setToDate(app.getTimeSlot().getStartDate());
			showDay(appModel.getAppointments(model.getYear(), model.getMonth(), model.getDay()));
			createCalendar();
			refreshScreen();
		}
		else if (opCode == 2)
		{
			showInfoMessage("Group appointment deleted.", "Appointment Deleted");
		}
	}

	public static void main(String[] args)
	{
		ClientController controller = new ClientController();
	}

	//all the action listeners
	private class DayButtonListener implements ActionListener
	{
		private int day;
		private List<Appointment> apps;
		public DayButtonListener(int day, List<Appointment> apps)
		{
			this.day = day;
			this.apps = apps;
		}

		public void actionPerformed(ActionEvent e)
		{
			model.setDay(day);
			model.setToDate(model.getMonth(), model.getYear(), day);
			if (!apps.isEmpty())
			{
				dayPane.removeAll();
				showDay(apps);
				refreshScreen();
			}
			
			else
			{
				dayPane.removeAll();
				refreshScreen();
			}
		}
	}

	private class NextListener implements ActionListener
	{
		private Appointment app;
		private int id;
		public NextListener(Appointment app)
		{
		      this.app = app;
			  if(app!=null)
				  id = app.getId();
			  else
				  id = -1;
		}

		public void actionPerformed(ActionEvent e)
		{
			String message = checker.checkGroupAppointment(v.size()-1,durMin.getText(), description.getText(), location.getText());
			if (message.equals(""))
			{

				int dur = Integer.parseInt(durMin.getText());
				Date ed = (Date) eDate.getModel().getValue();
				Date ld = (Date) lDate.getModel().getValue();
				List<String> usernames = new ArrayList<String>();
				try
				{
					if(app != null)
					{
						usernames = client.getGroupUsers(app.getId());
						changeGroupAppointment(app);						
					}
					else
					{
						for (UserData user : selUsers)
						{
							usernames.add(user.getLoginName());
						}
					}
					List<Period> periods = client.getTimeSlots(usernames, dur, ed, ld);
					
					if(!periods.isEmpty())
					{
						showTimeSlots(id, periods, usernames, location.getText(), description.getText());
						refreshScreen();
					}
					else
					{
						showErrorMessage("No time slot available between the chosen dates.");
						refreshScreen();
					}

				}

				catch (GroupwareClientException ex)
				{
					showErrorMessage(ex.getMessage());
				}
			}

			else
			{
				showErrorMessage(message);
			}
		}
	}

	private class DateComboBoxListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			model.setToDate(monthBox.getSelectedIndex(), Integer.parseInt((String) yearBox.getSelectedItem()));
			createCalendar();
			refreshScreen();
		}
	}

	private class UserComboBoxListener implements ActionListener
	{
		private int i = 0;

		public void actionPerformed(ActionEvent e)
		{
			//for some strange reason it generates an action
			//event when it is first created
			if (i > 0)
			{
				addOrDelToDelBox(userBox.getSelectedIndex());
			}
			if (i == 0)
			{
				i++;
			}
		}
	}
	
	private class DeleteComboBoxListener implements ActionListener
	{
		List<Appointment> apps;
		private int i = 0;
		public DeleteComboBoxListener(List<Appointment> apps)
		{
			this.apps = apps;
		}
		public void actionPerformed(ActionEvent e)
		{
			if (i > 0)
			{
				if(JOptionPane.showConfirmDialog(frame, "Are you sure you want to delete appointment "
					+ delete.getSelectedItem() + "?") == JOptionPane.YES_OPTION)
				{
					try
					{
						int id = apps.get(delete.getSelectedIndex()).getId();

						if (apps.get(delete.getSelectedIndex()).isGroupAppointment())
							client.deleteGroupAppointment(id);
						else
							client.deleteAppointment(id);

						appModel.setAppointments(client.getAllAppointments());
					}
					catch (GroupwareClientException ex)
					{
						showErrorMessage(ex.getMessage());
					}

					if (appModel.getAppointments(model.getYear(), model.getMonth(), model.getDay()).size() != 0)
						showDay(appModel.getAppointments(model.getYear(), model.getMonth(), model.getDay()));
					else
						dayPane.removeAll();

					createCalendar();
					refreshScreen();
				}
			}
			if (i == 0)
			{
				i++;
			}
		}
	}
	
	private class EditComboBoxListener implements ActionListener
	{
		private int i = 0;
		private List<Appointment> apps;
		public EditComboBoxListener(List<Appointment> apps)
		{
			this.apps = apps;
		}

		public void actionPerformed(ActionEvent e)
		{
			//for some strange reason it generates an action
			//event when it is first created
			if (i > 0)
			{
				Appointment ap = apps.get(edit.getSelectedIndex());
				if (ap.isGroupAppointment())
					createGroupAppointment(ap);
				else
					createSingleAppointment(ap);	
				refreshScreen();
			}
			if (i == 0)
			{
				i++;
			}
		}
	}

	private class ItemCloseListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			try
			{
				client.logout();
			}
			catch (Exception ev)
			{}
			System.exit(0);
		}
	}

	private class ItemSingleAppointmentListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			//mode = 0;			
			try
			{
				model.setGranularity(client.getGranularity());				
			}
			catch (GroupwareClientException ex)
			{}
			createSingleAppointment(null);
			refreshScreen();
		}
	}

	private class ItemGroupAppointmentListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			//mode = 0;			
			try
			{
				model.setGranularity(client.getGranularity());				
			}
			catch (GroupwareClientException ex)
			{}
			createGroupAppointment(null);
			refreshScreen();
		}
	}

	private class SingleAppointmentListener implements ActionListener
	{
		Appointment app;
		public SingleAppointmentListener(Appointment app)
		{
			this.app = app;
		}
		public void actionPerformed(ActionEvent e)
		{

			    int day = model.getDay();
				Date olddate = model.getDate();
				//model.setToWantedDate();
				Date bt = (Date) bTime.getModel().getValue();				
				Date et = (Date) eTime.getModel().getValue();
				model.setToDate(bt);
				int bhours = model.getHour();
				int bmins  = model.getMinute();
				model.setToDate(et);
				int ehours = model.getHour();
				int emins  = model.getMinute();
				Date bd    = model.getDateForAppointment(Integer.parseInt((String) yearBox.getSelectedItem()), monthBox.getSelectedIndex(), day, bhours, bmins);
				Date ed    = model.getDateForAppointment(Integer.parseInt((String) yearBox.getSelectedItem()), monthBox.getSelectedIndex(), day, ehours, emins);
				try
				{
					if(app==null)
					{					      
					         client.createSingleAppointment(description.getText(), location.getText(), bd, ed);
					              showInfoMessage("Appointment created successfully", "Appointment added");
					}
					
					else
					{
						app.setLocation(location.getText());
						app.setDescription(description.getText());
						app.setTimeSlot(new Period(bd, ed));
						app.setGroupAppointment(false);
				        client.changeAppointment(app);
			            showInfoMessage("Appointment changed successfully", "Appointment changed");
					}
					appModel.setAppointments(client.getAllAppointments());
					appointmentPane.removeAll();
					model.setToDate(olddate);
					showDay(appModel.getAppointments(model.getYear(), model.getMonth(), model.getDay()));
					createCalendar();					
					refreshScreen();

				}
				catch (GroupwareClientException ex)
				{
					showErrorMessage(ex.getMessage());
				}
		}
	}

	private class ItemGranularityListener implements ActionListener
	{
		int gran;

		public ItemGranularityListener(int gran)
		{
			this.gran = gran;
		}

		public void actionPerformed(ActionEvent e)
		{
			try
			{
				client.setGranularity(gran);
			}
			catch (GroupwareClientException ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}
	}

	private class LoginListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (checker.checkLoginFields(loginName.getText(), password.getPassword()))
			{
				try
				{					
					client.login(loginName.getText(), new String(password.getPassword()), appModel);
					emptyScreen();
					createMainScreen();
					refreshScreen();
				}
				catch (GroupwareClientException ex)
				{
					showErrorMessage(ex.getMessage());
				}
			}
			else
			{
				showErrorMessage("Some fields are empty");
			}
		}
	}

	private class CreateListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			emptyScreen();
			createAccountScreen();
			refreshScreen();
		}
	}

	private class ItemDeleteListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (JOptionPane.showConfirmDialog(frame, "Are you sure you want to delete your account?") == JOptionPane.YES_OPTION)
			{
				try
				{
					client.deleteCurrentAccount();
					emptyScreen();
					frame.setJMenuBar(null);
					menuBar.removeAll();
					createLoginScreen();
					refreshScreen();
				}
				catch (GroupwareClientException ev)
				{
					showErrorMessage(ev.getMessage());
				}
			}
		}
	}

	private class TimeSlotListener implements ActionListener
	{
		private List<Period> periods;
		private List<String> users;
		private String des, loc;
		private int id;

		public TimeSlotListener(int id, List<Period> periods, List<String> users, String des, String loc)
		{
			this.periods = periods;
			this.users = users;
			this.des = des;
			this.loc = loc;
			this.id  = id;
		}

		public void actionPerformed(ActionEvent e)
		{			
			try
			{
				if(id < 0)
				{
				    client.createGroupAppointment(users, des, loc, periods.get(bTimeBox.getSelectedIndex()));
				    showInfoMessage("Group appointment has been added successfully", "Appointment created");
				}
				
				else
				{
				    client.moveGroupAppointment(id, periods.get(bTimeBox.getSelectedIndex()));
				    showInfoMessage("Group appointment changed successfully", "Appointment changed");
				}
				appModel.setAppointments(client.getAllAppointments());
				appointmentPane.removeAll();				
				createCalendar();
				model.setToDate(periods.get(bTimeBox.getSelectedIndex()).getStartDate());
				showDay(appModel.getAppointments(model.getYear(), model.getMonth(), model.getDay()));
				refreshScreen();
			}
			catch (GroupwareClientException ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}
	}

	private class SubmitGroupChangeListener implements ActionListener
	{
		private Appointment app;
		public SubmitGroupChangeListener(Appointment app)
		{
			this.app = app;
		}
		public void actionPerformed(ActionEvent e)
		{
			try
			{
				model.setToDate(app.getTimeSlot().getStartDate());
                changeGroupAppointment(app);
	            showInfoMessage("Appointment changed successfully", "Appointment changed");
			    appModel.setAppointments(client.getAllAppointments());					
				appointmentPane.removeAll();
				showDay(appModel.getAppointments(model.getYear(), model.getMonth(), model.getDay()));
				createCalendar();				
				refreshScreen();
			}
			catch (Exception ex){showErrorMessage(ex.getMessage());}

		}
	}
	
	private class ItemLogoutListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			try
			{
				client.logout();
			}
			catch (Exception ev)
			{}
			emptyScreen();
			frame.setJMenuBar(null);
			menuBar.removeAll();
			createLoginScreen();
			refreshScreen();
		}
	}

	private class CreateAccountListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String pass1 = new String(p1.getPassword());
			String pass2 = new String(p2.getPassword());

			if (!pass1.equals(pass2))
			{
				showErrorMessage("The passwords don't match");
				return;
			}

			if (!checker.checkAccountFields(name.getText(), loginC.getText(), title.getText(),
				phone.getText(), p1.getPassword(), p2.getPassword()))
			{
				showErrorMessage("Some fields are empty or not correct");
				return;
			}

			try
			{
				// Richard, you don't need createAccount to return a boolean. If the mehod finishes
				// execution, then it means all was ok. Otherwise, a GroupwareClientException is thrown,
				// so you can know in this way that there was a problem with creating the account.
				client.createAccount(new UserData(loginC.getText(), name.getText()), pass1,
					Granularity.THIRTY_MINUTES, phone.getText(), title.getText());

				showInfoMessage("You have successfully created a new account. " +
					"You will now be logged in automatically.", "Account created");

				client.login(loginC.getText(), pass1, appModel);

				emptyScreen();
				createMainScreen();
				refreshScreen();
			}
			catch (GroupwareClientException ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}
	}
}