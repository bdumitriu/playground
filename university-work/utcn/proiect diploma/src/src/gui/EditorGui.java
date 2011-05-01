package gui;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.awt.*;
import java.util.ArrayList;
import java.util.Observer;
import java.util.Observable;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.net.MalformedURLException;
import java.io.FileWriter;
import java.io.IOException;

import comm.repository.*;
import comm.dusync.DocumentServer;
import comm.dusync.DocumentServerImpl;
import comm.dusync.PermissionException;
import data.EditorUser;
import data.Datetime;
import enum.OperationType;
import enum.UpdateType;
import enum.TreeLevel;
import gui.diag.CheckOutVersionDialog;
import gui.diag.FindVersionDialog;
import gui.diag.RMIDialog;
import utils.Debugger;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Feb 26, 2004
 * Time: 6:05:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class EditorGui implements DocumentListener, ActionListener, Observer, UserListFunctions
{
	class StyleModifier implements Runnable
	{
		private int offset;
		private int size;
		private JTextPane text;
		private SimpleAttributeSet attr;

		public StyleModifier(JTextPane text, int offset, int size, SimpleAttributeSet attr)
		{
			this.offset = offset;
			this.size = size;
			this.text = text;
			this.attr = attr;
		}

		public void run()
		{
			text.getStyledDocument().setCharacterAttributes(offset, size, attr, true);
		}
	}

	public static void main(String arg[])
	{
		if (arg.length == 1 && arg[0].equals("--debug"))
		{
			Debugger.getInstance().setDebugging(true);
		}
		new EditorGui();
	}

	public EditorGui()
	{
		mainFrame = new JFrame("Asynchronous Collaborative Editor 0.0.1");

		text = new JTextPane();
		//doc = new core.Document("", mainFrame, text);
		doc = new core.Document("", mainFrame);

		text.setText(doc.getText());
		text.getDocument().addDocumentListener(this);
		text.setEditable(false);

		version = new JLabel();

		// create the menu
		menuBar = new JMenuBar();

		menuFile = new JMenu("File");
		menuFile.setMnemonic(KeyEvent.VK_F);
		menuOps = new JMenu("Operations");
		menuOps.setMnemonic(KeyEvent.VK_O);

		menuBar.add(menuFile);
		menuBar.add(menuOps);

		itemFileSaveAs = new JMenuItem("Save as...", KeyEvent.VK_S);
		itemFileExit = new JMenuItem("Exit", KeyEvent.VK_E);

		itemOpsUpdate = new JMenuItem("Update", KeyEvent.VK_U);
		itemOpsUpdate.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_U, InputEvent.CTRL_DOWN_MASK));
		itemOpsCommit = new JMenuItem("Commit", KeyEvent.VK_C);
		itemOpsCommit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK));
		itemOpsCheckOutVersion = new JMenuItem("Check out version", KeyEvent.VK_O);
		itemOpsFindVersionByDate = new JMenuItem("Find version by date", KeyEvent.VK_F);

		menuFile.add(itemFileSaveAs);
		menuFile.addSeparator();
		menuFile.add(itemFileExit);

		menuOps.add(itemOpsUpdate);
		menuOps.add(itemOpsCommit);
		menuOps.addSeparator();
		menuOps.add(itemOpsCheckOutVersion);
		menuOps.add(itemOpsFindVersionByDate);

		itemFileSaveAs.addActionListener(this);
		itemFileSaveAs.setActionCommand(cmdSaveAs);
		itemFileExit.addActionListener(this);
		itemFileExit.setActionCommand(cmdExit);
		itemOpsUpdate.addActionListener(this);
		itemOpsUpdate.setActionCommand(cmdUpdate);
		itemOpsCommit.addActionListener(this);
		itemOpsCommit.setActionCommand(cmdCommit);
		itemOpsCheckOutVersion.addActionListener(this);
		itemOpsCheckOutVersion.setActionCommand(cmdCheckOut);
		itemOpsFindVersionByDate.addActionListener(this);
		itemOpsFindVersionByDate.setActionCommand(cmdFindVersion);

		// create the action buttons
		commitButton = new JButton("Commit");
		commitButton.setIcon(new ImageIcon("img/commit.png"));
		commitButton.addActionListener(this);
		commitButton.setActionCommand(cmdCommit);

		updateButton = new JButton("Update");
		updateButton.setIcon(new ImageIcon("img/update.png"));
		updateButton.addActionListener(this);
		updateButton.setActionCommand(cmdUpdate);

		// create the radio buttons
		noChoiceRB = new JRadioButton("automatic");
		noChoiceRB.setActionCommand(cmdNoChoice);
		noChoiceRB.addActionListener(this);
		conflictChoiceRB = new JRadioButton("conflict unit comparison");
		conflictChoiceRB.setActionCommand(cmdConflictChoice);
		conflictChoiceRB.addActionListener(this);
		operationChoiceRB = new JRadioButton("operation comparison");
		operationChoiceRB.setActionCommand(cmdOperationChoice);
		operationChoiceRB.addActionListener(this);
		noChoiceRB.setSelected(true);

		wordLevelRB = new JRadioButton("word level");
		sentenceLevelRB = new JRadioButton("sentence level");
		paragraphLevelRB = new JRadioButton("paragraph level");
		wordLevelRB.setSelected(true);

		ButtonGroup choiceGroup = new ButtonGroup();
		choiceGroup.add(noChoiceRB);
		choiceGroup.add(conflictChoiceRB);
		choiceGroup.add(operationChoiceRB);

		ButtonGroup conflictGroup = new ButtonGroup();
		conflictGroup.add(wordLevelRB);
		conflictGroup.add(sentenceLevelRB);
		conflictGroup.add(paragraphLevelRB);

		// create the user list component
		userList = new UserListGui(mainFrame, this);

		// create the layout objects
		JPanel buttonPanel = new JPanel();
		JPanel otherPanel = new JPanel(new BorderLayout());

		JScrollPane jsp = new JScrollPane(text);
		jsp.setPreferredSize(new Dimension(600, 400));

		buttonPanel.add(commitButton);
		buttonPanel.add(updateButton);

		JPanel conflictGroupPanel = new JPanel(new GridLayout(0, 1));
		JPanel choiceGroupPanel = new JPanel(new GridLayout(0, 1));

		conflictGroupPanel.add(new JLabel("Conflict level:"));
		conflictGroupPanel.add(wordLevelRB);
		conflictGroupPanel.add(sentenceLevelRB);
		conflictGroupPanel.add(paragraphLevelRB);

		choiceGroupPanel.add(new JLabel("Conflict resolution method:"));
		choiceGroupPanel.add(noChoiceRB);
		choiceGroupPanel.add(conflictChoiceRB);
		choiceGroupPanel.add(operationChoiceRB);

		Box radioBox = Box.createHorizontalBox();
		radioBox.add(conflictGroupPanel);
		radioBox.add(choiceGroupPanel);
		radioBox.add(Box.createHorizontalGlue());

		JPanel versionPanel = new JPanel();

		versionPanel.add(new JLabel("Your version: "));
		versionPanel.add(version);

		otherPanel.add(versionPanel, BorderLayout.NORTH);
		otherPanel.add(userList, BorderLayout.CENTER);

		mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(radioBox, BorderLayout.NORTH);
		mainPanel.add(jsp, BorderLayout.CENTER);
		mainPanel.add(buttonPanel, BorderLayout.SOUTH);
		mainPanel.add(otherPanel, BorderLayout.EAST);

		mainFrame.setJMenuBar(menuBar);
		mainFrame.getContentPane().add(mainPanel);
		mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		mainFrame.pack();
		mainFrame.show();

		RMIDialog dialog = new RMIDialog(mainFrame);
		dialog.show();

		rmiServer = dialog.getRmiServer();
		rmiPort = dialog.getRmiPort();

		text.setEditable(false);
		initStyle();


		text.setCharacterAttributes(localtextStyle, true);

		update();

		text.setEditable(true);

		registerRMI();
	}

	public void initStyle()
	{
		localtextStyle = new SimpleAttributeSet();
		StyleConstants.setFontSize(localtextStyle, 12);
		StyleConstants.setForeground(localtextStyle, Color.black);

		remotetextStyle = new SimpleAttributeSet();
		StyleConstants.setFontSize(localtextStyle, 12);
		StyleConstants.setForeground(remotetextStyle, Color.blue);
	}

	public void registerRMI()
	{
		// registering RMI server
		try
		{
			//int rmiport = 1100;
			DocumentServer ds = new DocumentServerImpl(doc);

			// try to create a new RMI registry on rmiPort and, if this fails, try to find an already
			// running one
			Registry reg = null;
			try
			{
				reg = LocateRegistry.createRegistry(rmiPort);
			}
			catch (RemoteException e)
			{
				reg = LocateRegistry.getRegistry(rmiPort);
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

	public static void reapplyStyles(JTextPane c, Style style)
	{
		// Get section element
		Element sectionElem = c.getDocument().getDefaultRootElement();

		// Get number of paragraphs.
		int paraCount = sectionElem.getElementCount();

		for (int i = 0; i < paraCount; i++)
		{
			Element paraElem = sectionElem.getElement(i);
			AttributeSet attr = paraElem.getAttributes();

			// Get the name of the style applied to this paragraph element; may be null
			String sn = (String) attr.getAttribute(StyleConstants.NameAttribute);

			// Check if style name match
			if (style.getName().equals(sn))
			{
				// Reapply the paragraph style
				int rangeStart = paraElem.getStartOffset();
				int rangeEnd = paraElem.getEndOffset();
				c.getStyledDocument().setParagraphAttributes(rangeStart, rangeEnd - rangeStart, style, true);
			}

			// Enumerate the content elements
			for (int j = 0; j < paraElem.getElementCount(); j++)
			{
				Element contentElem = paraElem.getElement(j);
				attr = contentElem.getAttributes();

				// Get the name of the style applied to this content element; may be null
				sn = (String) attr.getAttribute(StyleConstants.NameAttribute);

				// Check if style name match
				if (style.getName().equals(sn))
				{
					// Reapply the content style
					int rangeStart = contentElem.getStartOffset();
					int rangeEnd = contentElem.getEndOffset();
					c.getStyledDocument().setCharacterAttributes(rangeStart, rangeEnd - rangeStart, style, true);
				}
			}
		}
	}

	public void updateText(EditorUser editorUser)
	{
		Style style = text.getStyle(editorUser.getIp());
		if (style == null) return;

		StyleConstants.setForeground(style, editorUser.getColor());
		reapplyStyles(text, style)  ;
	}

	public void actionPerformed(ActionEvent e)
	{
		String cmd = e.getActionCommand();
		if (cmd.equals(cmdSaveAs))
		{
			saveAs();
		}
		if (cmd.equals(cmdExit))
		{
			exit();
		}
		else if (cmd.equals(cmdUpdate))
		{
			update();
		}
		else if (cmd.equals(cmdCommit))
		{
			commit();
		}
		else if (cmd.equals(cmdCheckOut))
		{
			String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

			try
			{
				Repository remoteRef = (Repository) Naming.lookup(url);
				int repVersion = remoteRef.getCurrentVersion();

				CheckOutVersionDialog covDialog = new CheckOutVersionDialog(mainFrame, repVersion);

				covDialog.show();

				checkOutVersion(covDialog.getVersion());
			}
			catch (NotBoundException e1)
			{
				JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
					JOptionPane.ERROR_MESSAGE);
			}
			catch (MalformedURLException e1)
			{
				JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
					JOptionPane.ERROR_MESSAGE);
			}
			catch (RemoteException e1)
			{
				JOptionPane.showMessageDialog(mainFrame, "RMI error in comunication. Check server " +
					"and port.", "Error", JOptionPane.ERROR_MESSAGE);
			}
		}
		else if (cmd.equals(cmdFindVersion))
		{
			findVersionByDate();
		}
		else if (cmd.equals(cmdNoChoice))
		{
			userList.setSynchButtonStyle(true);
		}
		else if (cmd.equals(cmdConflictChoice))
		{
			userList.setSynchButtonStyle(false);
		}
		else if (cmd.equals(cmdOperationChoice))
		{
			userList.setSynchButtonStyle(false);
		}
	}

	public void synchronize(String ip, boolean asMaster)
	{
		String url = "rmi://" + ip + ":1099/docserver";
		try
		{
			DocumentServer docserRef = (DocumentServer) Naming.lookup(url);
			ArrayList ops = docserRef.getOperations(doc.getCurrentVersion());

			doc.addObserver(this);
			doc.synchronize(ops, getConflictLevel(), getConflictResolutionType(), asMaster);
			doc.deleteObserver(this);

		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact remote gui.", "Error",
					JOptionPane.ERROR_MESSAGE);
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact remote gui.", "Error",
					JOptionPane.ERROR_MESSAGE);
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "RMI error in comunication..", "Error",
					JOptionPane.ERROR_MESSAGE);
		}
		catch (PermissionException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Permission denied.",
					"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Not the same version.",
					"Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	private void saveAs()
	{
		JFileChooser chooser = new JFileChooser();
		int r = chooser.showSaveDialog(mainFrame);
		if (r == JFileChooser.APPROVE_OPTION)
		{
			System.out.println(chooser.getSelectedFile().getName());
			try
			{
				FileWriter fw = new FileWriter(chooser.getSelectedFile().getAbsolutePath());
				fw.write(text.getText());
				fw.close();
			}
			catch (IOException e)
			{
				JOptionPane.showMessageDialog(mainFrame, "Failed to write output file.",
					"Error", JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	private void exit()
	{
		System.exit(0);
	}

	public boolean firstupdate = true;

	private void update()
	{
		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);
			int repVersion = remoteRef.getCurrentVersion();

			if (repVersion == doc.getCurrentVersion())
			{
				if (!firstupdate)
				{
					JOptionPane.showMessageDialog(mainFrame, "You are already up to date.", "Info",
						JOptionPane.INFORMATION_MESSAGE);
				}

				firstupdate = false;
				return;
			}

			if (!firstupdate) doc.addObserver(this);

			for (int i = doc.getCurrentVersion(); i < repVersion; i++)
			{
				ArrayList remoteLog = remoteRef.getOperations(i, i + 1);
				doc.update(remoteLog, getConflictLevel(), getConflictResolutionType());
				doc.setCurrentVersion(doc.getCurrentVersion() + 1);
			}

			if (!firstupdate)
			{
				doc.deleteObserver(this);
			}

			text.getDocument().removeDocumentListener(this);
			if (firstupdate)
			{
				int len = text.getStyledDocument().getLength();
				text.getStyledDocument().remove(0, len);
				text.getStyledDocument().insertString(0, doc.getText(), localtextStyle);
				text.setText(doc.getText());
			}
			text.getDocument().addDocumentListener(this);

			if (!firstupdate)
			{
				JOptionPane.showMessageDialog(mainFrame, "Update successful.", "Info",
					JOptionPane.INFORMATION_MESSAGE);
			}

			version.setText(new Integer(doc.getCurrentVersion()).toString());

			firstupdate = false;
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "The version sent to the repository seems to be " +
				"corrupt. Please restart program.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (RepositoryException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "The repository was unable to send the information.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (BadLocationException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	private void commit()
	{

		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);

			if (remoteRef.getCurrentVersion() != doc.getCurrentVersion())
			{
				JOptionPane.showMessageDialog(mainFrame, "The repository version has changed.\n" +
					"Please update before committing.", "Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else if (remoteRef.commit(doc.getCommitLog(), doc.getCurrentVersion()))
			{
				doc.setCurrentVersion(doc.getCurrentVersion() + 1);
				doc.emptyLog();
				JOptionPane.showMessageDialog(mainFrame, "Commit successful.", "Info",
					JOptionPane.INFORMATION_MESSAGE);

				version.setText(new Integer(doc.getCurrentVersion()).toString());
			}
			else
			{
				JOptionPane.showMessageDialog(mainFrame, "The repository version has changed.\n" +
					"Please update before committing.", "Info", JOptionPane.INFORMATION_MESSAGE);
			}
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "The version sent to the repository seems to be " +
				"corrupt. Please restart program.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (RepositoryException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "The repository was unable to commit your data.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	private void checkOutVersion(int checkOutVersion)
	{
		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);

			//doc = new core.Document("", mainFrame, text);
			doc = new core.Document("", mainFrame);

			for (int i = doc.getCurrentVersion(); i < checkOutVersion; i++)
			{
				ArrayList remoteLog = remoteRef.getOperations(i, i + 1);
				doc.update(remoteLog, getConflictLevel(), getConflictResolutionType());
				doc.setCurrentVersion(doc.getCurrentVersion() + 1);
			}

			text.getDocument().removeDocumentListener(this);
			text.setText(doc.getText());
			text.getDocument().addDocumentListener(this);

			version.setText(new Integer(doc.getCurrentVersion()).toString());
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "The version sent to the repository seems to be " +
				"corrupt. Please restart program.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (RepositoryException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "The repository was unable to send the information.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	private void findVersionByDate()
	{
		FindVersionDialog fvDialog = new FindVersionDialog(mainFrame);

		fvDialog.show();

		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);

			Datetime timestamp = new Datetime();
			timestamp.setYear(fvDialog.getYear());
			timestamp.setMonth(fvDialog.getMonth());
			timestamp.setDay(fvDialog.getDay());
			timestamp.setHour(fvDialog.getHour());
			timestamp.setMinute(fvDialog.getMinute());
			timestamp.setSecond((byte) 0);
			timestamp.setMilli((short) 0);

			int ver = remoteRef.getVersionByTimestamp(timestamp);

			if (ver == -1)
			{
				JOptionPane.showMessageDialog(mainFrame, "All versions on repository was committed " +
					"before the date you specified.", "Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				JOptionPane.showMessageDialog(mainFrame, "The version number closest to date you " +
					"specified is " + ver + ".", "Info", JOptionPane.INFORMATION_MESSAGE);
			}
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(mainFrame, "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	public void changedUpdate(DocumentEvent e)
	{}

	public void insertUpdate(DocumentEvent e)
	{
		int offset = e.getOffset();

		for (int i = 0; i < e.getLength(); i++)
			doc.insertChar(text.getText().charAt(offset + i), offset + i);

		StyleModifier sm = new StyleModifier(text, offset, e.getLength(), localtextStyle);

		Thread t = new Thread (sm);
		t.start();

		if (!text.getText().equals(doc.getText()))
		{
			System.out.println("--text--");
			System.out.println(text.getText());
			System.out.println("--doc--");
			System.out.println(doc.getText());
			System.out.println("Something's wrong!");
		}
	}

	public void removeUpdate(DocumentEvent e)
	{
		int offset = e.getOffset();

		for (int i = 0; i < e.getLength(); i++)
			doc.deleteChar(offset);

		if (!text.getText().equals(doc.getText()))
		{
			System.out.println("--text--");
			System.out.println(text.getText());
			System.out.println("--doc--");
			System.out.println(doc.getText());
			System.out.println("Something's wrong!");
		}
	}

	public void update(Observable o, Object arg)
	{
		text.getDocument().removeDocumentListener(this);

		AbsoluteOperation absOp = (AbsoluteOperation) arg;

		//System.out.println(absOp);

		try
		{
			if (absOp.getType() == OperationType.delete)
			{
				text.getStyledDocument().remove(absOp.getPosition(), absOp.getSize());
			}
			else
			if (absOp.getType() == OperationType.insert)
			{
				String ip = absOp.getSourceIp();
				Style style = text.getStyle(ip);
				EditorUser eusr =  userList.getUserList().getEditorUserByIp(ip);
				if (style == null)
				{
					style = text.addStyle(ip, null);
					StyleConstants.setFontSize(style, 12);
					if (eusr == null)	StyleConstants.setForeground(style, Color.blue);
					else StyleConstants.setForeground(style, eusr.getColor());
				}

				text.getStyledDocument().insertString(absOp.getPosition(), absOp.getContent(), style);

			}
		}
		catch (BadLocationException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}

		text.getDocument().addDocumentListener(this);

	}

	/**
	 * Returns the user selected conflict level as a TreeLevel constant.
	 *
	 * @return the user selected conflict level as a TreeLevel constant
	 */
	public int getConflictLevel()
	{
		if (wordLevelRB.isSelected())
		{
			return TreeLevel.word;
		}
		else if (sentenceLevelRB.isSelected())
		{
			return TreeLevel.sentence;
		}
		else if (paragraphLevelRB.isSelected())
		{
			return TreeLevel.paragraph;
		}

		return -1;
	}

	/**
	 * Returns the user selected conflict resolution type as a UpdateType constant.
	 *
	 * @return the user selected conflict resolution type as a UpdateType constant
	 */
	public int getConflictResolutionType()
	{
		if (noChoiceRB.isSelected())
		{
			return UpdateType.noChoice;
		}
		else if (conflictChoiceRB.isSelected())
		{
			return UpdateType.conflictChoice;
		}
		else if (operationChoiceRB.isSelected())
		{
			return UpdateType.operationChoice;
		}

		return -1;
	}

	private JFrame mainFrame;
	private JTextPane text;
	private JPanel mainPanel;
	private JMenuBar menuBar;

	private JMenu menuFile;
	private JMenu menuOps;

	private JMenuItem itemFileSaveAs;
	private JMenuItem itemFileExit;
	private JMenuItem itemOpsUpdate;
	private JMenuItem itemOpsCommit;
	private JMenuItem itemOpsCheckOutVersion;
	private JMenuItem itemOpsFindVersionByDate;

	private JButton commitButton;
	private JButton updateButton;

	private JRadioButton wordLevelRB;
	private JRadioButton sentenceLevelRB;
	private JRadioButton paragraphLevelRB;

	private JRadioButton noChoiceRB;
	private JRadioButton conflictChoiceRB;
	private JRadioButton operationChoiceRB;

	private UserListGui userList;

	private JLabel version;

	private String rmiServer;
	private int rmiPort;

	private final String cmdSaveAs = "SavaAs";
	private final String cmdExit = "Exit";
	private final String cmdUpdate = "Update";
	private final String cmdCommit = "Commit";
	private final String cmdCheckOut = "CheckOut";
	private final String cmdFindVersion = "FindVersion";

	private final String cmdNoChoice = "NoChoice";
	private final String cmdConflictChoice = "ConflictChoice";
	private final String cmdOperationChoice = "OperationChoice";

	private core.Document doc;

	private SimpleAttributeSet localtextStyle;
	private SimpleAttributeSet remotetextStyle;
}