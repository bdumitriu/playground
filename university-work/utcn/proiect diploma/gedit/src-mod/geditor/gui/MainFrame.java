package geditor.gui;

import geditor.tools.Bundle;
import geditor.engine.synch.DocumentServer;
import geditor.engine.synch.DocumentServerImpl;

import javax.swing.*;
import java.awt.*;
import java.net.URL;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 24, 2003
 * Time: 12:06:28 PM
 */


public class MainFrame extends JFrame implements UserListFunctions
{
	private DrawToolBar drawToolBar;
	private StandardButtonToolbar standardButtonToolBar;
	private MyMenuBar menuBar;
	private StatusBar statusbar;

	private ObjectInspector objectInspector;
	private UsersToolbar usersToolbar;
	private OperationsToolBar operationsToolBar;
	private UserListGui userListGui;
	private InteractionChoice intChoice;

//	private HistoryFrame historyFrame;
//	private ConflictFrame conflictFrame;


	private JLabel versionLabel;

	private WorkArea workArea;

	private static MainFrame singletonInstance  = new MainFrame("Asynchronous Graphical Editor v0.1");

	public static MainFrame getInstance()
	{
		return singletonInstance;
	}

	private MainFrame(String title)
	{
		// frame initialization
		super(title);

		workArea = new WorkArea();

		setSize(new Dimension(800, 640));
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		if (frameSize.height > screenSize.height)
		{
			frameSize.height = screenSize.height;
		}
		if (frameSize.width > screenSize.width)
		{
			frameSize.width = screenSize.width;
		}
		setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);

		//creating graphic documnent
		//grdoc = new GraphicDocument();

		//create desktop object

		//create draw  & standard butt. toolbar

		drawToolBar = new DrawToolBar();
		drawToolBar.setStatus(DrawToolBar.ACTIVATED);

		//create standard button bar
		//standardButtonToolBar = new StandardButtonToolbar(this);
		// create menubar
		menuBar = new MyMenuBar();
		setJMenuBar(menuBar);
		// create statusbar
		statusbar = new StatusBar(this);
		// create object inspector
		objectInspector = new ObjectInspector();
		// create userToollbar
		usersToolbar = new UsersToolbar();
		// create opreation toolbar
		operationsToolBar = new OperationsToolBar();
		// create user list gui
		userListGui = new UserListGui(this, this);
		// create the interaction choice
		intChoice = new InteractionChoice();

		//create the history frame
		//historyFrame = new HistoryFrame(this);
		//create the conflict history frame
		//conflictFrame = new ConflictFrame(this);

		versionLabel = new JLabel("Version: 0");

		// additional componets
		JPanel panelNorth = new JPanel(new BorderLayout());
		panelNorth.add(drawToolBar, BorderLayout.WEST);
		panelNorth.add(operationsToolBar, BorderLayout.CENTER);

		// additional componets
		JPanel panelEast = new JPanel(new BorderLayout());
		panelEast.add(objectInspector, BorderLayout.CENTER);
		panelEast.add(userListGui, BorderLayout.SOUTH);
		panelEast.add(versionLabel, BorderLayout.NORTH);

		//add components to the frame
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(panelNorth, BorderLayout.NORTH);

		getContentPane().add(intChoice, BorderLayout.WEST);
		getContentPane().add(panelEast, BorderLayout.EAST);
		getContentPane().add(statusbar, BorderLayout.SOUTH);


		statusbar.display("Welcome to this graphical editor. Version 1.0");

		setIconImage((new ImageIcon("resources/images/file.gif")).getImage());

		setVisible(true);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		operationsToolBar.setStatus(OperationsToolBar.ACTIVATED);

		RMIDialog rmiDiag = new RMIDialog(this);
		rmiDiag.show();

		workArea.getDocument().setRmiServer(rmiDiag.getRmiServer());
		workArea.getDocument().setRmiPort(rmiDiag.getRmiPort());

		registerRMI();
	}

	public void initDrawboard()
	{
/*
		FileController fileController = new FileController(application,  "file");
		application.getOpenedFileList().put("file", fileController);

		DrawBoard fr = new DrawBoard(application,  fileController);
		fileController.setWorkArea(fr.getWorkArea());*/
		getContentPane().add(workArea, BorderLayout.CENTER);
		drawToolBar.getFgColorBar().setColor(Color.black);
		drawToolBar.getBgColorBar().setColor(Color.white);

		getWorkArea().getDocument().update();
		getWorkArea().repaint();
	}

	public void registerRMI()
	{
		// registering RMI server
		try
		{
			int rmiport = 1099;
			DocumentServer ds = new DocumentServerImpl(workArea.getDocument());

			// try to create a new RMI registry on rmiPort and, if this fails, try to find an already
			// running one
			Registry reg = null;
			try
			{
				reg = LocateRegistry.createRegistry(rmiport);
			}
			catch (RemoteException e)
			{
				reg = LocateRegistry.getRegistry(rmiport);
			}

			// bind the Repository server to the name "repository" in the newly created RMI server
			reg.rebind("docserver", ds);
		}
		catch (RemoteException e)
		{
			e.printStackTrace();
			System.out.println("A RemoteException has occured. Aborting execution.");
		}
	}

	public void doAbout()
	{
		new HelpFrame();
	}

	public void doVersion()
	{
		//group objects
		URL img = null;//Application.class.getResource("resources/images/ethlogo.gif");
		String imagesrc = "<img src=\"" + img + "\">";
		String version = Bundle.getString("version");
		JOptionPane.showMessageDialog(this, "<html>" + imagesrc + "<br> <center><font size=4 fgColor=red>GEditor version " +
			version + "</font><br><font size=3 fgColor=blue>Made by Lorant Csaszar<br>2003 ETH Zurich</font><br></center></html>", "Product version", JOptionPane.CLOSED_OPTION, null);
	}

	public void doClose()
	{
		//close the current file
//        ((FileController) application.getOpenedFileList().get(application.getCurrentFileID())).doClose();
	}

	public void doStartDemo()
	{
		// ((FileController) application.getOpenedFileList().get(application.getCurrentFileID())).doStartDemo();
	}

	public void doCloseAll()
	{
		/*
	    Vector fname = new Vector();
	    Enumeration enumeration = application.getOpenedFileList().keys();
	    while (enumeration.hasMoreElements()) {
		String key = (String) enumeration.nextElement();
		fname.add(key);
	    }

	    for (int i = 0; i < fname.size(); i++) {
		((FileController) application.getOpenedFileList().get(fname.get(i))).doClose();
	    }
	      */

	}

	public void doSettings()
	{

	}

	public void doSave()
	{
//        ((FileController) application.getOpenedFileList().get(application.getCurrentFileID())).saveFile();
	}

	public void doQuit()
	{
		doCloseAll();

/*        if (application.getNetworkOperationMode() == Application.NETWORK_WEB_MODE) {
            new TerminaterWEB(application,"",TerminaterWEB.CLOSE_APPLCATION);
        }*/
	}

	public void synchronize(String ip, boolean asMaster)
	{
		workArea.getDocument().synchronize(ip, asMaster);
	}

	public StatusBar getStatusBar()
	{
		return statusbar;
	}

	public DrawToolBar getDrawToolBar()
	{
		return drawToolBar;
	}

	public StandardButtonToolbar getStandardButtonToolbar()
	{
		return standardButtonToolBar;
	}

	public UsersToolbar getUserToolBar()
	{
		return usersToolbar;
	}

	public ObjectInspector getObjectInspector()
	{
		return objectInspector;
	}

	public OperationsToolBar getOperationToolBar()
	{
		return operationsToolBar;
	}

	public MyMenuBar getMyMenuBar()
	{
		return menuBar;
	}

//	public HistoryFrame getHistoryFrame()
//	{
//		return historyFrame;
//	}

	public WorkArea getWorkArea()
	{
		return workArea;
	}

	public UserListGui getUserListGui()
	{
		return userListGui;
	}

//	public ConflictFrame getConflictFrame()
//	{
//		return conflictFrame;
//	}

	public void setVersionLabel(int version)
	{
		versionLabel.setText("Version  : " + version);
	}
}

