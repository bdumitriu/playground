package gui;

import bm.EntryManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Aug 11, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class ApplicationGui extends JFrame implements ActionListener
{
	public static void main(String[] args)
	{
		new ApplicationGui().setVisible(true);
	}

	public ApplicationGui()
	{
		frameLayout = new CardLayout();

		modified = false;
		entryManager = new EntryManager(new File("data/da-dict.xml"));

		setTitle("Dutch Learning Aid 0.1");
		setBounds(100, 100, 640, 730);

		addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				if (modified)
				{
					int desire = JOptionPane.showOptionDialog(ApplicationGui.this, "Dictionary has " +
						"been modified.\nDo you wish to save the changes to disk?",
						"Save changes?", JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE, null, null, null);

					if (desire == JOptionPane.YES_OPTION)
					{
						entryManager.save();
						JOptionPane.showMessageDialog(ApplicationGui.this, "Dictionary has " +
							"been saved to disk.", "Success", JOptionPane.INFORMATION_MESSAGE);
					}
				}

				System.exit(0);
			}
		});

		getContentPane().setLayout(frameLayout);

		JMenuBar menuBar = new JMenuBar();

		JMenu mngmtMenu = new JMenu("Data management");
		mngmtMenu.setMnemonic(KeyEvent.VK_M);
		JMenu displayMenu = new JMenu("Data display");
		displayMenu.setMnemonic(KeyEvent.VK_D);

		JMenuItem insWordItem = new JMenuItem("Insert word");
		insWordItem.setActionCommand(insWordCom);
		insWordItem.setMnemonic(KeyEvent.VK_I);
		insWordItem.addActionListener(this);
		JMenuItem editWordItem = new JMenuItem("Modify word");
		editWordItem.setActionCommand(editWordCom);
		editWordItem.setMnemonic(KeyEvent.VK_M);
		editWordItem.addActionListener(this);
		JMenuItem saveDictItem = new JMenuItem("Save dictionary");
		saveDictItem.setActionCommand(saveDictCom);
		saveDictItem.setMnemonic(KeyEvent.VK_S);
		saveDictItem.addActionListener(this);

		JMenuItem listWordsItem = new JMenuItem("List all words");
		listWordsItem.setActionCommand(listWordsCom);
		listWordsItem.setMnemonic(KeyEvent.VK_A);
		listWordsItem.addActionListener(this);

		menuBar.add(mngmtMenu);
		menuBar.add(displayMenu);

		mngmtMenu.add(insWordItem);
		mngmtMenu.add(editWordItem);
		mngmtMenu.add(new JSeparator());
		mngmtMenu.add(saveDictItem);

		displayMenu.add(listWordsItem);

		setJMenuBar(menuBar);

		insWordScreen = new InsertWordScreen(this);
		editWordScreen = new EditWordScreen(this);
		listWordsScreen = new ListWordsScreen(this);

		getContentPane().add(insWordScr, insWordScreen);
		getContentPane().add(editWordScr, editWordScreen);
		getContentPane().add(listWordsScr, listWordsScreen);
	}

	public void setModified(boolean modified)
	{
		this.modified = modified;
	}

	public EntryManager getEntryManager()
	{
		return entryManager;
	}

	/**
	 * Displays the edit screen and loads the entry identified by <code>dutchWord</code> for editing.
	 */
	public void showEditScreenFor(String dutchWord)
	{
		editWordScreen.updateDisplayedWord(dutchWord);
		frameLayout.show(getContentPane(), editWordScr);
		editWordScreen.focusDefaultComponent();
	}

	public void actionPerformed(ActionEvent e)
	{
		String actionCommand = e.getActionCommand();
		if (actionCommand.equals(insWordCom))
		{
			frameLayout.show(getContentPane(), insWordScr);
			insWordScreen.focusDefaultComponent();
		}
		else if (actionCommand.equals(editWordCom))
		{
			frameLayout.show(getContentPane(), editWordScr);
			editWordScreen.focusDefaultComponent();
		}
		else if (actionCommand.equals(saveDictCom))
		{
			entryManager.save();
			modified = false;
			JOptionPane.showMessageDialog(this, "Dictionary has been saved to disk.", "Success",
				JOptionPane.INFORMATION_MESSAGE);
		}
		else if (actionCommand.equals(listWordsCom))
		{
			frameLayout.show(getContentPane(), listWordsScr);
			listWordsScreen.focusDefaultComponent();
		}
	}

	private CardLayout frameLayout;
	private EntryManager entryManager;

	// used in order to know whether or not the dictionary has been modified
	private boolean modified;

	private InsertWordScreen insWordScreen;
	private EditWordScreen editWordScreen;
	private ListWordsScreen listWordsScreen;

	public static final String insWordCom = "insertWordCommand";
	public static final String editWordCom = "editWordCommand";
	public static final String saveDictCom = "saveDictCommand";
	public static final String listWordsCom = "listWordsCommand";

	public static final String insWordScr = "insertWordScreen";
	public static final String editWordScr = "editWordScreen";
	public static final String listWordsScr = "listWordsScreen";
}
