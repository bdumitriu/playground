package ass1.view;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import ass1.controller.Controller;

public class AddBookWindow extends JDialog {

	private static final long serialVersionUID = 1L;

	public AddBookWindow(JFrame parent, final Controller controller) {
		super(parent, "Add book", true);

		final JTextField titleTF = new JTextField();
		final JTextField authorTF = new JTextField();
		final JTextField isbnTF = new JTextField();
		final JTextField publisherTF = new JTextField();
		final JTextField nrPagesTF = new JTextField();

		JPanel panel = new JPanel(new GridLayout(5, 2));
		panel.setLayout(new GridLayout(5, 2));

		panel.add(new JLabel("Title:"));
		panel.add(titleTF);
		panel.add(new JLabel("Author:"));
		panel.add(authorTF);
		panel.add(new JLabel("ISBN:"));
		panel.add(isbnTF);
		panel.add(new JLabel("Publisher:"));
		panel.add(publisherTF);
		panel.add(new JLabel("Number of pages:"));
		panel.add(nrPagesTF);

		getContentPane().add(panel, BorderLayout.CENTER);

		JButton addButton = new JButton("Add");
		addButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				controller.addBook(titleTF.getText(), authorTF.getText(),
						isbnTF.getText(), publisherTF.getText(),
						Integer.parseInt(nrPagesTF.getText()));
				AddBookWindow.this.dispose();
			}
		});

		getContentPane().add(addButton, BorderLayout.SOUTH);
	}
}
