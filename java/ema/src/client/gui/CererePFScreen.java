package client.gui;

import shared.EMAGuiLabels;
import shared.EMATooltips;
import shared.EMAErrorMessages;

import javax.swing.*;
import java.util.GregorianCalendar;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import data.RecordPF;
import data.RecordTipCerere;
import data.RecordCalitate;
import data.RecordSolicitant;
import client.comm.CererePFData;
import client.exception.CommunicationException;
import client.exception.SystemException;
import client.comm.CererePF;
import client.gui.iv.IntegerInputVerifier;

/**
 * This class handles the GUI part of the Cerere PF (individual request) part of the application.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 13, 2003
 */
public class CererePFScreen extends DisplayAdapter
{
	public CererePFScreen()
	{
		dataHandler = new CererePFData();
	}

	public void onShow(JFrame parent)
	{
		this.parent = parent;

		try
		{
			initDisplay();
		}
		catch (CommunicationException e)
		{
			JOptionPane.showMessageDialog(parent, e.getMessage(),
			        EMAErrorMessages.getInstance().getMessage("error.name"), JOptionPane.ERROR_MESSAGE);
			System.exit(1);
		}
		catch (SystemException e)
		{
			JOptionPane.showMessageDialog(parent, e.getMessage(),
			        EMAErrorMessages.getInstance().getMessage("error.name"), JOptionPane.ERROR_MESSAGE);
			System.exit(1);
		}

		parent.validate();

		caScreen = null;
		caScreenRequested = false;
	}

	public void onHide(JFrame parent)
	{
		// if a request for the creation of a CerereApometreScreen has been made, create it, hide this screen
		// and display the newly created one
		if (caScreenRequested == true)
		{
			// put all data in a CererePF object
			CererePF cerereData = new CererePF();

			cerereData.setCodCerere(new Integer(codTF.getText()).intValue());
			cerereData.setAnCerere(((Integer) anSpinner.getValue()).intValue());

			cerereData.setNume(numeTF.getText());
			cerereData.setAdresa(adresaTF.getText());
			cerereData.setTelefon(telefonTF.getText());

			for (int i = 0; i < tipuriCerere.length; i++)
			{
				if (tipuriCerere[i].getNume().equals(tipCerereCombo.getSelectedItem()))
				{
					cerereData.setIdTipCerere(tipuriCerere[i].getId());
				}
			}

			for (int i = 0; i < calitati.length; i++)
			{
				if (calitati[i].getNume().equals("detinator"))
				{
					cerereData.setIdCalitate(calitati[i].getId());
				}
			}

			for (int i = 0; i < solicitanti.length; i++)
			{
				if (solicitanti[i].getNume().equals("persoana fizica"))
				{
					cerereData.setIdSolicitant(solicitanti[i].getId());
				}
			}

			cerereData.setZiDD(((Integer) ziDDSpinner.getValue()).intValue());
			cerereData.setLunaDD(((Integer) lunaDDSpinner.getValue()).intValue());
			cerereData.setAnDD(((Integer) anDDSpinner.getValue()).intValue());

			cerereData.setZiDS(((Integer) ziDSSpinner.getValue()).intValue());
			cerereData.setLunaDS(((Integer) lunaDSSpinner.getValue()).intValue());
			cerereData.setAnDS(((Integer) anDSSpinner.getValue()).intValue());

			caScreen = new CerereApometreScreen(cerereData);
			caScreenRequested = false;
			super.onHide(parent);
			caScreen.onShow(parent);
		}
		else
		{
			// if no CerereApometreScreen has been created, just hide this screen
			if (caScreen == null)
			{
				super.onHide(parent);
			}
			// if a CerereApometreScreen has been created, forward the request to that screen
			else
			{
				caScreen.onHide(parent);
			}
		}
	}

	private void initDisplay() throws CommunicationException, SystemException
	{
		// get current day, month & year
		GregorianCalendar calendar = new GregorianCalendar();
		int day = calendar.get(GregorianCalendar.DAY_OF_MONTH);
		int month = calendar.get(GregorianCalendar.MONTH) + 1;
		int year = calendar.get(GregorianCalendar.YEAR);

		Integer step = new Integer(1);

		// init spinner values for day
		Integer valueDay = new Integer(day);
		Integer minDay = new Integer(calendar.getActualMinimum(GregorianCalendar.DAY_OF_MONTH));
		Integer maxDay = new Integer(calendar.getActualMaximum(GregorianCalendar.DAY_OF_MONTH));

		// init spinner models for day
		ziDDModel = new SpinnerNumberModel(valueDay, minDay, maxDay, step);
		ziDSModel = new SpinnerNumberModel(valueDay, minDay, maxDay, step);

		// init spinner values for month
		Integer valueMonth = new Integer(month);
		Integer minMonth = new Integer(calendar.getActualMinimum(GregorianCalendar.MONTH) + 1);
		Integer maxMonth = new Integer(calendar.getActualMaximum(GregorianCalendar.MONTH) + 1);

		// init spinner models for months
		lunaDDModel = new SpinnerNumberModel(valueMonth, minMonth, maxMonth, step);
		lunaDSModel = new SpinnerNumberModel(valueMonth, minMonth, maxMonth, step);

		// init spinner values for year
		Integer valueYear = new Integer(year);
		Integer minYear = new Integer(year - 2);
		Integer maxYear = new Integer(year + 2);

		// init spinner models for years
		anModel = new SpinnerNumberModel(valueYear, minYear, maxYear, step);
		anDDModel = new SpinnerNumberModel(valueYear, minYear, maxYear, step);
		anDSModel = new SpinnerNumberModel(valueYear, minYear, maxYear, step);

		// create components
		codTF = new JTextField(4);
		anSpinner = new JSpinner(anModel);
		codTF.setInputVerifier(new IntegerInputVerifier());

		numeTF = new SelectionList(SelectionList.CUSTOM_BEHAVIOUR, null, new CererePFSelectionListener());
		numeTF.setColumns(15);
		adresaTF = new JTextField(30);
		telefonTF = new JTextField(15);

		ziDDSpinner = new JSpinner(ziDDModel);
		lunaDDSpinner = new JSpinner(lunaDDModel);
		anDDSpinner = new JSpinner(anDDModel);

		ziDSSpinner = new JSpinner(ziDSModel);
		lunaDSSpinner = new JSpinner(lunaDSModel);
		anDSSpinner = new JSpinner(anDSModel);

		tipCerereCombo = new JComboBox();
		tipuriCerere = dataHandler.getTipuriCerere();
		calitati = dataHandler.getCalitati();
		solicitanti = dataHandler.getSolicitanti();

		if ((tipuriCerere == null) || (tipuriCerere.length == 0))
		{
			throw new SystemException(EMAErrorMessages.getInstance().getMessage("error.noTipuriCerere"));
		}

		if ((calitati == null) || (calitati.length == 0))
		{
			throw new SystemException(EMAErrorMessages.getInstance().getMessage("error.noCalitati"));
		}

		if ((solicitanti == null) || (solicitanti.length == 0))
		{
			throw new SystemException(EMAErrorMessages.getInstance().getMessage("error.noSolicitanti"));
		}

		for (int i = 0; i < tipuriCerere.length; i++)
		{
			tipCerereCombo.addItem(tipuriCerere[i].getNume());
		}

		EMAGuiLabels gl = EMAGuiLabels.getInstance();
		EMATooltips tt = EMATooltips.getInstance();

		// create the action buttons
		nextButton = new JButton(gl.getMessage("gui.button.next"));
		nextButton.setToolTipText(tt.getMessage("tooltip.button.next"));
		nextButton.addActionListener(new ButtonListener());

		// create the layout components
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new GridBagLayout());
		JScrollPane scrollPane = new JScrollPane(mainPanel);
		parent.getContentPane().add(scrollPane);

		GridBagConstraints gbc = new GridBagConstraints();

		// add the title label
		JLabel titleLabel = new JLabel(gl.getMessage("gui.label.cererePF"));
		titleLabel.setFont(titleLabel.getFont().deriveFont((float) 22));

		gbc.fill = GridBagConstraints.NONE;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTH;
		gbc.insets.bottom = 20;

		mainPanel.add(titleLabel, gbc);

		// add the cod/an (code/year) fields
		gbc.fill = GridBagConstraints.NONE;
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.WEST;

		mainPanel.add(new JLabel(gl.getMessage("gui.label.codInregistrare") + ": "), gbc);
		gbc.anchor = GridBagConstraints.CENTER;
		mainPanel.add(codTF, gbc);
		mainPanel.add(new JLabel(" / "), gbc);

		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTHWEST;

		mainPanel.add(anSpinner, gbc);

		// add the nume (name) fields
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.WEST;

		mainPanel.add(new JLabel(gl.getMessage("gui.label.numePF") + ": "), gbc);

		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTHWEST;

		mainPanel.add(numeTF, gbc);

		// add the adresa (address) fields
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.WEST;

		mainPanel.add(new JLabel(gl.getMessage("gui.label.adresaPF") + ": "), gbc);

		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTHWEST;

		mainPanel.add(adresaTF, gbc);

		// add the telefon (telephone) fields
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.WEST;

		mainPanel.add(new JLabel(gl.getMessage("gui.label.telefonPF") + ": "), gbc);

		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTHWEST;

		mainPanel.add(telefonTF, gbc);

		// add the tip cerere (request type) fields
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.WEST;

		mainPanel.add(new JLabel(gl.getMessage("gui.label.tipCerere") + ": "), gbc);

		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTHWEST;

		mainPanel.add(tipCerereCombo, gbc);

		// add the data cerere (request date) fields
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.WEST;

		mainPanel.add(new JLabel(gl.getMessage("gui.label.dataDepunere") + ": "), gbc);
		gbc.anchor = GridBagConstraints.CENTER;
		mainPanel.add(ziDDSpinner, gbc);
		mainPanel.add(new JLabel(" . "), gbc);
		mainPanel.add(lunaDDSpinner, gbc);
		mainPanel.add(new JLabel(" . "), gbc);

		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTHWEST;

		mainPanel.add(anDDSpinner, gbc);

		// add the data scadentei (due date) fields
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.WEST;

		mainPanel.add(new JLabel(gl.getMessage("gui.label.dataScadenta") + ": "), gbc);
		gbc.anchor = GridBagConstraints.CENTER;
		mainPanel.add(ziDSSpinner, gbc);
		mainPanel.add(new JLabel(" . "), gbc);
		mainPanel.add(lunaDSSpinner, gbc);
		mainPanel.add(new JLabel(" . "), gbc);

		gbc.gridwidth = GridBagConstraints.REMAINDER;
		gbc.anchor = GridBagConstraints.NORTHWEST;

		mainPanel.add(anDSSpinner, gbc);

		// add the "next" button
		gbc.anchor = GridBagConstraints.SOUTHEAST;

		mainPanel.add(nextButton, gbc);
	}

	class ButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() == nextButton)
			{
				int cod = -1;
				try
				{
					cod = new Integer(codTF.getText()).intValue();
				}
				catch (NumberFormatException exception)
				{
					return;
				}

				boolean codCerereExists = false;
				try
				{
					codCerereExists = dataHandler.checkCodCerere(cod,
						((Integer) anSpinner.getValue()).intValue());
				}
				catch (CommunicationException ex)
				{
					JOptionPane.showMessageDialog(parent, ex.getMessage(),
						EMAErrorMessages.getInstance().getMessage("error.name"),
					        JOptionPane.ERROR_MESSAGE);
					return;
				}

				if (codCerereExists == true)
				{
					JOptionPane.showMessageDialog(parent,
						EMAErrorMessages.getInstance().getMessage("error.duplicateCodCerere"),
			                        EMAErrorMessages.getInstance().getMessage("error.name"),
					        JOptionPane.ERROR_MESSAGE);
					return;
				}
				else
				{
					caScreenRequested = true;
					CererePFScreen.this.onHide(parent);
				}
			}
		}
	}

	class CererePFSelectionListener implements SelectionListListener
	{
		public void selectionIndexChanged(SelectionListEvent selListEvent)
		{
			adresaTF.setText(records[selListEvent.getNewIndex()].getAdresa());
			telefonTF.setText(records[selListEvent.getNewIndex()].getTelefon());
		}

		public void entrySelected(SelectionListEvent selListEvent)
		{
			numeTF.setText(records[selListEvent.getNewIndex()].getNume());
		}

		public String[] getListFor(String text)
		{

			try
			{
				records = dataHandler.getListFor(text);
			}
			catch (CommunicationException e)
			{
				records = null;
			}

			if (records == null)
			{
				return null;
			}

			String[] selList = new String[records.length];
			for (int i = 0; i < records.length; i++)
			{
				selList[i] = records[i].getNume();
			}

			return selList;
		}
	}

	/**
	 * A reference to the object handling all the businnes logic of this component.
	 */
	private CererePFData dataHandler;

	/**
	 * Always contains the last returned record list in the getListFor method.
	 */
	RecordPF[] records;

	/**
	 * The parent frame.
	 */
	private JFrame parent;

	/**
	 * Controls for request book keeping info.
	 */
	private JTextField codTF;
	private JSpinner anSpinner;
	private SpinnerNumberModel anModel;

	/**
	 * Controls for personal info.
	 */
	private SelectionList numeTF;
	private JTextField adresaTF;
	private JTextField telefonTF;

	/**
	 * Controls for data depunerii cererii (request submission date).
	 */
	private JSpinner ziDDSpinner;
	private JSpinner lunaDDSpinner;
	private JSpinner anDDSpinner;
	private SpinnerNumberModel ziDDModel;
	private SpinnerNumberModel lunaDDModel;
	private SpinnerNumberModel anDDModel;

	/**
	 * Controls for data scandentei cererii (request due date).
	 */
	private JSpinner ziDSSpinner;
	private JSpinner lunaDSSpinner;
	private JSpinner anDSSpinner;
	private SpinnerNumberModel ziDSModel;
	private SpinnerNumberModel lunaDSModel;
	private SpinnerNumberModel anDSModel;

	/**
	 * Controls for tip cerere (request type).
	 */
	private JComboBox tipCerereCombo;

	/**
	 * Action buttons for the screen.
	 */
	private JButton nextButton;

	/**
	 * The CerereApometreScreen to forward the onHide call to.
	 */
	private CerereApometreScreen caScreen;

	/**
	 * Variable showing that a request to display the CerereApometreScreen has been made.
	 */
	private boolean caScreenRequested;

	/**
	 * Variables for holding the respective entries in the database.
	 */
	private RecordTipCerere[] tipuriCerere;
	private RecordCalitate[] calitati;
	private RecordSolicitant[] solicitanti;
}