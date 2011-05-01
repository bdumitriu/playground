package client.gui;

import shared.EMAGuiLabels;
import shared.EMATooltips;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 13, 2003
 */
public class EMAClient
{
	public static void main(String[] args)
	{
		new EMAClient();
	}

	public EMAClient()
	{
		gl =  EMAGuiLabels.getInstance();
		currentDisplay = null;

		mainFrame = new JFrame(gl.getMessage("gui.mainframe.title"));
		mainFrame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				System.exit(0);
			}
		});

		setFramePositionOnScreen();
		initMainMenu();
		setDisplay(new CererePFScreen());

		mainFrame.show();
	}

	/**
	 * Changes the current display to <code>screen</code>.
	 *
	 * @param screen the new display to show to the user
	 */
	public void setDisplay(Display screen)
	{
		if (currentDisplay != null)
		{
			currentDisplay.onHide(mainFrame);
		}

		currentDisplay = screen;
		currentDisplay.onShow(mainFrame);
	}

	/**
	 * Decide where to put the frame and how big to make it
	 */
	private void setFramePositionOnScreen()
	{
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		int height = (int) screenSize.getHeight() - 200;
		int width = (int) screenSize.getWidth() - 200;
		mainFrame.setBounds(100, 100, width, height);
	}

	/**
	 * Create and initialize all items of main menu & submenus.
	 */
	private void initMainMenu()
	{
		mainMenu = new JMenuBar();

		// build the file menu
		JMenu fileMenu = new JMenu(gl.getModifiedMessage("gui.mainmenu.file"));
		fileMenu.setMnemonic(gl.getMnemonicFrom("gui.mainmenu.file"));

		JMenuItem exitItem = new JMenuItem(new ExitAction());

		fileMenu.addSeparator();
		fileMenu.add(exitItem);

		// build the help menu
		JMenu helpMenu = new JMenu(gl.getModifiedMessage("gui.mainmenu.help"));
		helpMenu.setMnemonic(gl.getMnemonicFrom("gui.mainmenu.help"));

		JMenuItem helpItem = new JMenuItem(new HelpAction());
		JMenuItem aboutItem = new JMenuItem(new AboutAction());

		helpMenu.add(helpItem);
		helpMenu.addSeparator();
		helpMenu.add(aboutItem);

		// add all menus to main menu
		mainMenu.add(fileMenu);
		mainMenu.add(helpMenu);

		mainFrame.setJMenuBar(mainMenu);
	}

	/**
	 * The application's main frame.
	 */
	private JFrame mainFrame;

	/**
	 * The application's main menu.
	 */
	private JMenuBar mainMenu;

	/**
	 * The screen currently on display in this frame.
	 */
	private Display currentDisplay;

	/**
	 * A reference to the EMAGuiLabels class for obtaining the labels to use throughout the GUI.
	 */
	private EMAGuiLabels gl;
}

class ExitAction extends AbstractAction
{
	public ExitAction()
	{
		super();
		putValue(Action.NAME, EMAGuiLabels.getInstance().getModifiedMessage("gui.mainmenu.file.exit"));
		putValue(Action.MNEMONIC_KEY,
		        new Integer(EMAGuiLabels.getInstance().getMnemonicFrom("gui.mainmenu.file.exit")));
		putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.ALT_MASK));
		putValue(Action.SHORT_DESCRIPTION, EMATooltips.getInstance().getMessage("tooltip.mainmenu.file.exit"));
	}

	public void actionPerformed(ActionEvent e)
	{
		System.exit(0);
	}
}

class HelpAction extends AbstractAction
{
	public HelpAction()
	{
		super();
		putValue(Action.NAME, EMAGuiLabels.getInstance().getModifiedMessage("gui.mainmenu.help.help"));
		putValue(Action.MNEMONIC_KEY,
		        new Integer(EMAGuiLabels.getInstance().getMnemonicFrom("gui.mainmenu.help.help")));
		putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0));
		putValue(Action.SHORT_DESCRIPTION, EMATooltips.getInstance().getMessage("tooltip.mainmenu.help.help"));
	}

	public void actionPerformed(ActionEvent e)
	{
		// show application's help screen
	}
}

class AboutAction extends AbstractAction
{
	public AboutAction()
	{
		super();
		putValue(Action.NAME, EMAGuiLabels.getInstance().getModifiedMessage("gui.mainmenu.help.about"));
		putValue(Action.MNEMONIC_KEY,
		        new Integer(EMAGuiLabels.getInstance().getMnemonicFrom("gui.mainmenu.help.about")));
		putValue(Action.SHORT_DESCRIPTION, EMATooltips.getInstance().getMessage("tooltip.mainmenu.help.about"));
	}

	public void actionPerformed(ActionEvent e)
	{
		// show application's about screen
	}
}