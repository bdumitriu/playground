package ass3.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import ass3.factory.DomDirBuilder;
import ass3.factory.SaxDirBuilder;

/**
 * The GUI.
 *
 * @author bdumitriu
 */
public class Gui extends JFrame {

	private static final long serialVersionUID = 1L;

	private static final int H_SIZE = 520;
	private static final int V_SIZE = 240;

	JScrollPane scrollPane;

	public Gui() {
		setSize(H_SIZE, V_SIZE);
		setTitle("Directory Creator");
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((screenSize.width - H_SIZE) / 2,
				(screenSize.height - V_SIZE) / 2);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		getContentPane().setLayout(new BorderLayout());

		final JTextField xmlFile = new JTextField();
		xmlFile.setText("D:\\work\\workspace\\moo\\test.xml");
		JButton xmlFileButton = new JButton("Select...");

		xmlFileButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				JFileChooser xmlFileChooser = new JFileChooser();
				xmlFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				if (xmlFileChooser.showOpenDialog(Gui.this)
						== JFileChooser.APPROVE_OPTION) {
					xmlFile.setText(
							xmlFileChooser.getSelectedFile().getAbsolutePath());
				}
			}
		});

		final JTextField outputDir = new JTextField();
		outputDir.setText("D:\\personal\\test");
		JButton outputDirButton = new JButton("Select...");

		outputDirButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				JFileChooser outputDirChooser = new JFileChooser();
				outputDirChooser.setFileSelectionMode(
						JFileChooser.DIRECTORIES_ONLY);
				if (outputDirChooser.showOpenDialog(Gui.this)
						== JFileChooser.APPROVE_OPTION) {
					outputDir.setText(
							outputDirChooser.getSelectedFile().getAbsolutePath());
				}
			}
		});

		final JRadioButton saxButton = new JRadioButton("SAX");
		final JRadioButton domButton = new JRadioButton("DOM");
		ButtonGroup group = new ButtonGroup();

		saxButton.setSelected(true);
		group.add(saxButton);
		group.add(domButton);

		JButton runButton = new JButton("Run");
		runButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (saxButton.isSelected()) {
					new SaxDirBuilder().createDirectories(
							xmlFile.getText(), outputDir.getText());
				} else if (domButton.isSelected()) {
					new DomDirBuilder().createDirectories(
							xmlFile.getText(), outputDir.getText());
				}

				Navigator nav = new Navigator(outputDir.getText());
				if (scrollPane != null) {
					getContentPane().remove(scrollPane);
				}
				scrollPane = new JScrollPane(nav);
				getContentPane().add(scrollPane, BorderLayout.CENTER);
				getContentPane().validate();
			}
		});

		JPanel container = new JPanel();
		container.setLayout(new GridLayout(3, 3));

		container.add(new JLabel("Select the XML file:"));
		container.add(xmlFile);
		container.add(xmlFileButton);
		container.add(new JLabel("Select the output directory:"));
		container.add(outputDir);
		container.add(outputDirButton);
		container.add(saxButton);
		container.add(domButton);
		container.add(runButton);

		getContentPane().add(container, BorderLayout.NORTH);
	}

	public static void main(String[] args) {
		new Gui().setVisible(true);
	}
}
