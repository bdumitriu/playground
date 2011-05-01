package gui;

import bm.Entry;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.*;
import java.util.Enumeration;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Aug 12, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class EditWordScreen extends JPanel implements ActionListener
{
	/**
	 * Creates a new EditWordScreen with all its editing fields blank.
	 *
	 * @param parent the frame in which this screen is displayed
	 */
	public EditWordScreen(ApplicationGui parent)
	{
		this.parent = parent;

		setLayout(new BorderLayout());

		JPanel titlePanel = new JPanel();

		JLabel titleLabel = new JLabel("Edit word");
		titleLabel.setFont(new Font("Monospaced", Font.BOLD, 18));
		titleLabel.setHorizontalAlignment(JLabel.CENTER);

		titlePanel.add(titleLabel);

		Box mainBox = new Box(BoxLayout.Y_AXIS);

		Component strut = Box.createVerticalStrut(20);
		Box searchBox = Box.createHorizontalBox();
		Box dwordBox = Box.createHorizontalBox();
		Box rwordBox = Box.createHorizontalBox();
		Box ewordBox = Box.createHorizontalBox();
		Box typeBox = Box.createHorizontalBox();
		Box articleBox = Box.createHorizontalBox();
		Box freqBox = Box.createHorizontalBox();
		Box extraBox = Box.createHorizontalBox();
		Box sampleBox = Box.createHorizontalBox();

		JPanel searchPanel = new JPanel();
		JPanel dwordPanel = new JPanel();
		JPanel rwordPanel = new JPanel();
		JPanel ewordPanel = new JPanel();
		JPanel typePanel = new JPanel();
		JPanel articlePanel = new JPanel();
		JPanel freqPanel = new JPanel();
		JPanel extraPanel = new JPanel();
		JPanel samplePanel = new JPanel();

		mainBox.add(strut);
		mainBox.add(searchBox);
		mainBox.add(dwordBox);
		mainBox.add(rwordBox);
		mainBox.add(ewordBox);
		mainBox.add(typeBox);
		mainBox.add(articleBox);
		mainBox.add(freqBox);
		mainBox.add(extraBox);
		mainBox.add(sampleBox);

		JButton searchButton = new JButton("Find word");
		searchButton.setActionCommand(searchCom);
		searchButton.addActionListener(this);

		searchTf = new JTextField(20);
		dwordTf = new JTextField(20);
		rwordTf = new JTextField(20);
		ewordTf = new JTextField(20);
		articleBg = new ButtonGroup();
		freqBg = new ButtonGroup();
		extraTa = new JTextArea(3, 50);
		sampleTa = new JTextArea(3, 50);

		String[] types = {"noun", "verb", "adjective", "adverb", "pronoun", "numeral", "article", "preposition",
			"conjunction", "interjection"};
		typeCombo = new JComboBox(types);
		typeCombo.setActionCommand(typeComboCom);
		typeCombo.addActionListener(this);

		deButton = new JRadioButton("de");
		hetButton = new JRadioButton("het");
		noArticleButton = new JRadioButton("none");

		JPanel articleBgPanel = new JPanel(new GridLayout(3, 1));

		articleBgPanel.add(deButton);
		articleBgPanel.add(hetButton);
		articleBgPanel.add(noArticleButton);

		articleBg.add(deButton);
		articleBg.add(hetButton);
		articleBg.add(noArticleButton);
		deButton.setSelected(true);

		highFButton = new JRadioButton("high");
		mediumFButton = new JRadioButton("medium");
		lowFButton = new JRadioButton("low");

		JPanel freqBgPanel = new JPanel(new GridLayout(3, 1));

		freqBgPanel.add(highFButton);
		freqBgPanel.add(mediumFButton);
		freqBgPanel.add(lowFButton);

		freqBg.add(highFButton);
		freqBg.add(mediumFButton);
		freqBg.add(lowFButton);
		highFButton.setSelected(true);

		JScrollPane extraJsp = new JScrollPane(extraTa);
		JScrollPane sampleJsp = new JScrollPane(sampleTa);

		searchBox.add(searchPanel);
		searchPanel.add(searchTf);
		searchPanel.add(searchButton);

		dwordBox.add(dwordPanel);
		dwordPanel.add(new JLabel("Dutch word:"));
		dwordPanel.add(dwordTf);

		rwordBox.add(rwordPanel);
		rwordPanel.add(new JLabel("Romanian word:"));
		rwordPanel.add(rwordTf);

		ewordBox.add(ewordPanel);
		ewordPanel.add(new JLabel("English word:"));
		ewordPanel.add(ewordTf);

		typeBox.add(typePanel);
		typePanel.add(new JLabel("Type of word:"));
		typePanel.add(typeCombo);

		articleBox.add(articlePanel);
		articlePanel.add(articleLabel = new JLabel("Article:"));
		articlePanel.add(articleBgPanel);

		freqBox.add(freqPanel);
		freqPanel.add(new JLabel("Frequency:"));
		freqPanel.add(freqBgPanel);

		extraBox.add(extraPanel);
		extraPanel.add(new JLabel("Extra information:"));
		extraPanel.add(extraJsp);

		sampleBox.add(samplePanel);
		samplePanel.add(new JLabel("Sample sentence:"));
		samplePanel.add(sampleJsp);

		JPanel buttonPanel = new JPanel();

		JButton saveButton = new JButton("Save word");
		saveButton.setMnemonic(KeyEvent.VK_S);
		saveButton.setActionCommand(saveWordCom);
		saveButton.addActionListener(this);
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setActionCommand(cancelCom);
		cancelButton.addActionListener(this);

		buttonPanel.add(saveButton);
		buttonPanel.add(cancelButton);

		add(titleLabel, BorderLayout.NORTH);
		add(mainBox, BorderLayout.CENTER);
		add(buttonPanel, BorderLayout.SOUTH);
	}

	/**
	 * Updates the screen so that it displays the values associated with <code>word</code>
	 *
	 * @return true if <code>currentWord<code> exists in the dictionary, false otherwise
	 */
	public boolean updateDisplayedWord(String word)
	{
		currentWord = word;

		Entry entry = parent.getEntryManager().getEntry(currentWord);

		if (entry == null)
		{
			return false;
		}

		dwordTf.setText(entry.getDutchWord());
		rwordTf.setText(entry.getRomanianWord());
		ewordTf.setText(entry.getEnglishWord());

		typeCombo.setSelectedItem(entry.getType());

		String article = entry.getArticle();
		if (article.equals("het"))
		{
			hetButton.setSelected(true);
		}
		else if (article.equals("de"))
		{
			deButton.setSelected(true);
		}

		String freq = entry.getFrequency();
		if (freq.equals("high"))
		{
			highFButton.setSelected(true);
		}
		else if (freq.equals("medium"))
		{
			mediumFButton.setSelected(true);
		}
		else if (freq.equals("low"))
		{
			lowFButton.setSelected(true);
		}

		extraTa.setText(entry.getExtra());
		sampleTa.setText(entry.getSampleExpression());

		return true;
	}

	public void focusDefaultComponent()
	{
		searchTf.requestFocus();
	}

	public void actionPerformed(ActionEvent e)
	{
		String actionCommand = e.getActionCommand();
		if (actionCommand.equals(saveWordCom))
		{
			String dWord = dwordTf.getText();
			String rWord = rwordTf.getText();
			String eWord = ewordTf.getText();
			String extra = extraTa.getText();
			String sample = sampleTa.getText();

			String type = (String) typeCombo.getSelectedItem();

			String article = "";
			if (type.equals("noun"))
			{
				JRadioButton selButton = null;
				Enumeration enumeration = articleBg.getElements();
				while (enumeration.hasMoreElements())
				{
					JRadioButton curButton = (JRadioButton) enumeration.nextElement();
					if (curButton.isSelected())
					{
						selButton = curButton;
					}
				}
				article = selButton.getText();
				if (article.equals("none"))
				{
					article = "";
				}
			}

			JRadioButton selButton = null;
			Enumeration enumeration = freqBg.getElements();
			while (enumeration.hasMoreElements())
			{
				JRadioButton curButton = (JRadioButton) enumeration.nextElement();
				if (curButton.isSelected())
				{
					selButton = curButton;
				}
			}
			String freq = selButton.getText();

			boolean existed = parent.getEntryManager().modifyElement(currentWord, dWord, rWord, eWord, type,
				article, freq, extra, sample);

			if (!existed)
			{
				int answer = JOptionPane.showOptionDialog(parent, "Word does not exist in the " +
					"dictionary.\nDo you want to insert it as a new word?", "Add as new?",
					JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null, null, null);
				if (answer == JOptionPane.YES_OPTION)
				{
					parent.getEntryManager().addElement(dWord, rWord, eWord, type, article, freq,
						extra, sample);
					parent.setModified(true);
					JOptionPane.showMessageDialog(parent, "Word has been added to dictionary.",
						"Word added", JOptionPane.INFORMATION_MESSAGE);
				}
			}
			else
			{
				parent.setModified(true);
				JOptionPane.showMessageDialog(parent, "New word data has been saved.",
					"Modifications saved", JOptionPane.INFORMATION_MESSAGE);
			}
		}
		else if (actionCommand.equals(searchCom))
		{
			if (!updateDisplayedWord(searchTf.getText()))
			{
				JOptionPane.showMessageDialog(parent, "Word does not exist in the dictionary.",
					"Word not found", JOptionPane.INFORMATION_MESSAGE);
			}
		}
		else if (actionCommand.equals(cancelCom))
		{

		}
		else if (actionCommand.equals(typeComboCom))
		{
			if (((String) typeCombo.getSelectedItem()).equals("noun"))
			{
				articleLabel.setEnabled(true);
				deButton.setEnabled(true);
				hetButton.setEnabled(true);
				noArticleButton.setEnabled(true);
			}
			else
			{
				articleLabel.setEnabled(false);
				deButton.setEnabled(false);
				hetButton.setEnabled(false);
				noArticleButton.setEnabled(false);
			}
		}
	}

	private JTextField searchTf;

	private JTextField dwordTf;
	private JTextField rwordTf;
	private JTextField ewordTf;
	private JComboBox typeCombo;
	private JLabel articleLabel;
	private JRadioButton deButton;
	private JRadioButton hetButton;
	private JRadioButton noArticleButton;
	private ButtonGroup articleBg;
	JRadioButton highFButton;
	JRadioButton mediumFButton;
	JRadioButton lowFButton;
	private ButtonGroup freqBg;
	private JTextArea extraTa;
	private JTextArea sampleTa;

	private ApplicationGui parent;
	private String currentWord;

	public static final String typeComboCom = "typeComboCommand";
	public static final String searchCom = "searchCommand";
	public static final String saveWordCom = "saveWordCommand";
	public static final String cancelCom = "cancelCommand";
}
