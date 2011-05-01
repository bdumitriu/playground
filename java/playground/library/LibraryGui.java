
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Aug 1, 2004
 */
public class LibraryGui extends JFrame implements ActionListener
{
	private static final String addCommand = "add";
	private static final String removeCommand = "remove";
	private static final String editCommand = "edit";
	private static final String listCommand = "list";
	private static final String borrowCommand = "borrow";
	private static final String returnCommand = "return";

	private static final String blankInfo = "blank";
	private static final String addInfo = "add";
	private static final String removeInfo = "remove";
	private static final String editInfo = "edit";
	private static final String listInfo = "list";
	private static final String borrowInfo = "borrow";
	private static final String returnInfo = "return";

	public LibraryGui()
	{
		library = new Library();

		setBounds(100,100,640,480);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setTitle("Library");
		addWindowListener(new WindowAdapter()		//asociez un obiect care sa trateze evenimentele de inchidere a ferestrei
		{						// am nevoie de un obiect de tip WindowListener; nu vreau sa implementez met interf (7)
			public void windowClosing(WindowEvent e)   // impl doar ce vreau; pt asta scriu o clasa care sa ext WindowAdaptor, care la
			{					// randul ei impl met interf. Eu suprascriu doar ce ma intereseaza
				library.saveBooks();		// e o clasa anonimacare se declara precum se vede
			}
		});

		JMenuBar menu = new JMenuBar();  // JMenuBar e meniu principal pe orizontala

		JMenu mngMenu = new JMenu("Library Management");   // meniu secundar pe verticala
		JMenu opMenu = new JMenu("Library Operations");   // meniu secundar pe verticala

		JMenuItem addItem = new JMenuItem("Add Book");
		JMenuItem removeItem = new JMenuItem("Remove Book");
		JMenuItem editItem = new JMenuItem("Modify Book");
		JMenuItem listItem = new JMenuItem("List Books");

		JMenuItem borrowItem = new JMenuItem("Borrow Book");
		JMenuItem returnItem = new JMenuItem("Return Book");

		mngMenu.add(addItem);
		mngMenu.add(removeItem);
		mngMenu.add(editItem);
		mngMenu.add(new JSeparator());
		mngMenu.add(listItem);

		opMenu.add(borrowItem);
		opMenu.add(returnItem);

		menu.add(mngMenu);
		menu.add(opMenu);

		setJMenuBar(menu);			// ac met nu e precedata de obiect deoarece se refera la ob te tip JFrame (this)

		addItem.setActionCommand(addCommand);
		removeItem.setActionCommand(removeCommand);
		editItem.setActionCommand(editCommand);
		listItem.setActionCommand(listCommand);
		borrowItem.setActionCommand(borrowCommand);
		returnItem.setActionCommand(returnCommand);

		addItem.addActionListener(this);
		removeItem.addActionListener(this);     // this e un obiect ce apartine clasei curente si
		editItem.addActionListener(this);       // clasa curenta implementeaza interfata ...
		listItem.addActionListener(this);       // de aceea il pot da pe this ca parametru pt ca e instanta unei clase
		borrowItem.addActionListener(this);     // care implementeza interfata...
		returnItem.addActionListener(this);     // par cerut de functia addActionListener tre sa fie de tipul ActionListener

		CardLayout layout = new CardLayout();  // doar o componenta e vizibila la un moment dat - layoutul frameului
		Container contentPane = getContentPane();
		contentPane.setLayout(layout);    // setez layout doar pt. zona princ de desenare a JFrame (contentPane)

		BlankScreen blankScreen = new BlankScreen();
		AddScreen addScreen = new AddScreen(this);  //  in constr. this e ob care tocmai se creeaza, iar in metoda e ob cu care se apeleaza met.
		RemoveScreen removeScreen = new RemoveScreen(this);
		editScreen = new EditScreen(this);
		listScreen = new ListScreen(this);
		BorrowScreen borrowScreen = new BorrowScreen(this);
		ReturnScreen returnScreen = new ReturnScreen(this);

		contentPane.add(blankInfo, blankScreen);
		contentPane.add(addInfo, addScreen);
		contentPane.add(removeInfo, removeScreen);
		contentPane.add(editInfo, editScreen);
		contentPane.add(listInfo, listScreen);
		contentPane.add(borrowInfo, borrowScreen);
		contentPane.add(returnInfo, returnScreen);
	}

	public void actionPerformed(ActionEvent e)  // dc implementez o interfata trebuie sa-i impl. toate metodele definite de ea (Code-...)
	{
		CardLayout layout = (CardLayout) getContentPane().getLayout();
		if (e.getActionCommand().equals(addCommand))    // getActionComand intoarce un string care poate fi comparat doar cu equals, nu cu ==
		{
			layout.show(getContentPane(), addInfo);
		}
		else if (e.getActionCommand().equals(removeCommand))
		{
			layout.show(getContentPane(), removeInfo);
		}
		else if (e.getActionCommand().equals(editCommand))
		{
			editScreen.reset();
			layout.show(getContentPane(), editInfo);
		}
		else if (e.getActionCommand().equals(listCommand))
		{
			listScreen.updateBookList();
			layout.show(getContentPane(), listInfo);
		}
		else if (e.getActionCommand().equals(borrowCommand))
		{
			layout.show(getContentPane(), borrowInfo);
		}
		else if (e.getActionCommand().equals(returnCommand))
		{
			layout.show(getContentPane(), returnInfo);
		}
	}

	public Library getLibrary()
	{
		return library;
	}

	public static void main(String[] args)
	{
		LibraryGui gui = new LibraryGui();
		gui.show();
	}

	private Library library;
	private ListScreen listScreen;
	private EditScreen editScreen;
}
