package ass1.view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import ass1.controller.Controller;

public class ApplicationWindow extends JFrame {

	private static final long serialVersionUID = 1L;

	private Controller controller;

	public ApplicationWindow(Controller controller) {
		super("My Library");

		this.controller = controller;
		
		setBounds(400, 100, 400, 400);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		configureMenu();
	}

	private void configureMenu() {
		JMenuBar menuBar = new JMenuBar();

		JMenu fileMenu = new JMenu("File");

		JMenuItem saveItem = new JMenuItem("Save");
		JMenuItem exitItem = new JMenuItem("Exit");

		fileMenu.add(saveItem);
		fileMenu.addSeparator();		
		fileMenu.add(exitItem);

		JMenu manageMenu = new JMenu("Manage Books");

		JMenuItem addItem = new JMenuItem("Add book");
		JMenuItem deleteItem = new JMenuItem("Delete book");
		JMenuItem findItem = new JMenuItem("Find book");
		JMenuItem listItem = new JMenuItem("List books");

		manageMenu.add(addItem);		
		manageMenu.add(deleteItem);
		manageMenu.addSeparator();
		manageMenu.add(findItem);
		manageMenu.addSeparator();
		manageMenu.add(listItem);

		menuBar.add(fileMenu);
		menuBar.add(manageMenu);

		setJMenuBar(menuBar);

		saveItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				controller.save();
			}
		});

		exitItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				ApplicationWindow.this.dispose();
			}
		});

		addItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				AddBookWindow addBookWindow =
						new AddBookWindow(ApplicationWindow.this, controller);
				addBookWindow.pack();
				addBookWindow.setVisible(true);
			}
		});

		listItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				getContentPane().add(new ListBooksPanel(controller));
				validate();
			}
		});
	}
}
