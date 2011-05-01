package client.gui;

import shared.EMAGuiLabels;
import shared.EMATooltips;
import shared.EMAErrorMessages;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import java.util.ArrayList;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import client.comm.CerereApometreData;
import client.exception.CommunicationException;
import client.exception.SystemException;
import client.comm.Cerere;
import data.RecordFabricant;
import data.RecordDN;
import data.RecordApometru;

/**
 * This class handles the GUI part of the Introducere Date Apometre (watermeter data insertion) part of the application.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 25, 2003
 */
/*
 * TODO: add code to check the input in the "serie" & "clasa de exactitate" columns
 * TODO: add code to ensure that one & only one of "domeniu de interes public" & "domeniu de interes privat" is checked
 * TODO: make sure no rows are displayed if at least one Fabricant and at least one DN exists in the database
 * TODO: somehow help the user with the completion of the Serie of the Apometru
 */
public class CerereApometreScreen extends DisplayAdapter
{
	/**
	 * Builds a new CerereApometreScreen which will use the data in <code>data</code> as the cerere data.
	 * @param data
	 */
	public CerereApometreScreen(Cerere data)
	{
		dataHandler = new CerereApometreData();
		cerereData = data;
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

		parent.validate();
	}

	private void initDisplay() throws CommunicationException
	{
		initColumnNames();

		wmTableModel = new ApometreTableModel(1);
		wmTable = new JTable(wmTableModel)
		{
			// Implement table header tool tips.
			protected JTableHeader createDefaultTableHeader()
			{
				return new JTableHeader(columnModel)
				{
					public String getToolTipText(MouseEvent e)
					{
						String tip = null;

						int index = columnModel.getColumnIndexAtX(e.getPoint().x);
						int realIndex = columnModel.getColumn(index).getModelIndex();
						EMATooltips tt = EMATooltips.getInstance();

						switch (realIndex)
						{
							case 0:
								tip = tt.getMessage("tooltip.table.header.nrCrt");
								break;
							case 1:
								tip = tt.getMessage("tooltip.table.header.dn");
								break;
							case 2:
								tip = tt.getMessage("tooltip.table.header.fabricant");
								break;
							case 3:
								tip = tt.getMessage("tooltip.table.header.serie");
								break;
							case 4:
								tip = tt.getMessage("tooltip.table.header.clasa");
								break;
							case 5:
								tip = tt.getMessage("tooltip.table.header.dPrivat");
								break;
							case 6:
								tip = tt.getMessage("tooltip.table.header.dPublic");
								break;
							case 7:
								tip = tt.getMessage("tooltip.table.header.obs");
								break;
						}

						return tip;
					}
				};
			}
		};

		initColumnWidths();
		setupFabricantCombo();
		setupDNCombo();
		wmTableModel.setDefaultRowValues(0);

		EMAGuiLabels gl = EMAGuiLabels.getInstance();
		EMATooltips tt = EMATooltips.getInstance();

		// create the layout components
		JPanel mainPanel = new JPanel();
		JPanel buttonPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));
		parent.getContentPane().add(mainPanel);

		// add the table
		JScrollPane scrollPane = new JScrollPane(wmTable);

		// add the title label
		JLabel titleLabel = new JLabel(gl.getMessage("gui.label.introAp"));
		titleLabel.setFont(titleLabel.getFont().deriveFont((float) 22));
		titleLabel.setHorizontalAlignment(JLabel.CENTER);
		titleLabel.setBorder(new EmptyBorder(20, 0, 20, 0));

		mainPanel.add(titleLabel, BorderLayout.NORTH);
		mainPanel.add(scrollPane, BorderLayout.CENTER);
		mainPanel.add(buttonPanel, BorderLayout.SOUTH);

		// add the "Add row" button
		addRowButton = new JButton(gl.getMessage("gui.button.addRow"));
		deleteRowButton = new JButton(gl.getMessage("gui.button.delRow"));
		addProducerButton = new JButton(gl.getMessage("gui.button.addProd"));
		addDnButton = new JButton(gl.getMessage("gui.button.addDn"));
		saveButton = new JButton(gl.getMessage("gui.button.save"));

		// set the button's tooltips
		addRowButton.setToolTipText(tt.getMessage("tooltip.button.addRow"));
		deleteRowButton.setToolTipText(tt.getMessage("tooltip.button.delRow"));
		addProducerButton.setToolTipText(tt.getMessage("tooltip.button.addProd"));
		addDnButton.setToolTipText(tt.getMessage("tooltip.button.addDn"));
		saveButton.setToolTipText(tt.getMessage("tooltip.button.save"));

		buttonPanel.add(addRowButton);
		buttonPanel.add(deleteRowButton);
		buttonPanel.add(addProducerButton);
		//buttonPanel.add(addDnButton);
		buttonPanel.add(saveButton);

		ButtonListener buttonListener = new ButtonListener();
		addRowButton.addActionListener(buttonListener);
		deleteRowButton.addActionListener(buttonListener);
		addProducerButton.addActionListener(buttonListener);
		addDnButton.addActionListener(buttonListener);
		saveButton.addActionListener(buttonListener);
	}

	/**
	 * Sets up the combobox to be used as input for the Fabricant (watermeter producer) column.
	 */
	private void setupFabricantCombo() throws CommunicationException
	{
		RecordFabricant[] fabs = dataHandler.getFabricanti();

		if (fabs == null)
		{
			fabs = new RecordFabricant[0];
		}

		if (fabs.length == 0)
		{
			defaultFabricantValue = "";
		}
		else
		{
			defaultFabricantValue = fabs[0].getNume();
		}

		JComboBox comboBox = new JComboBox();

		for (int i = 0; i < fabs.length; i++)
		{
			comboBox.addItem(fabs[i].getNume());
		}

		wmTable.getColumnModel().getColumn(2).setCellEditor(new DefaultCellEditor(comboBox));
	}

	private void setupDNCombo() throws CommunicationException
	{
		RecordDN[] dns = dataHandler.getDNs();

		if (dns == null)
		{
			dns = new RecordDN[0];
		}

		if (dns.length == 0)
		{
			defaultDNValue = new Integer(-1);
		}
		else
		{
			defaultDNValue = new Integer(dns[0].getNumar());
		}

		JComboBox comboBox = new JComboBox();

		for (int i = 0; i < dns.length; i++)
		{
			comboBox.addItem(new Integer(dns[i].getNumar()));
		}

		wmTable.getColumnModel().getColumn(1).setCellEditor(new DefaultCellEditor(comboBox));
	}

	/**
	 * Initializes the names of the table's columns to predefined values.
	 */
	private void initColumnNames()
	{
		EMAGuiLabels gl = EMAGuiLabels.getInstance();

		columnNames = new String[NR_COLUMNS];

		columnNames[0] = gl.getMessage("gui.table.header.nrCrt");
		columnNames[1] = gl.getMessage("gui.table.header.dn");
		columnNames[2] = gl.getMessage("gui.table.header.fabricant");
		columnNames[3] = gl.getMessage("gui.table.header.serie");
		columnNames[4] = gl.getMessage("gui.table.header.clasa");
		columnNames[5] = gl.getMessage("gui.table.header.dPrivat");
		columnNames[6] = gl.getMessage("gui.table.header.dPublic");
		columnNames[7] = gl.getMessage("gui.table.header.obs");
	}

	/**
	 * Initializes the widths of all of the table's columns to predefined values.
	 */
	private void initColumnWidths()
	{
		wmTable.getColumnModel().getColumn(0).setPreferredWidth(20);
		wmTable.getColumnModel().getColumn(1).setPreferredWidth(20);
		wmTable.getColumnModel().getColumn(2).setPreferredWidth(150);
		wmTable.getColumnModel().getColumn(3).setPreferredWidth(150);
		wmTable.getColumnModel().getColumn(4).setPreferredWidth(30);
		wmTable.getColumnModel().getColumn(5).setPreferredWidth(30);
		wmTable.getColumnModel().getColumn(6).setPreferredWidth(50);
		//the last column (usu. observations) will get the rest
	}

	/**
	 * Returns the apometre (watermeters) entered in the table as an array of RecordApometru.
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 * @throws SystemException if the id's that have to be filled in in each RecordApometru cannot be properly
	 *      retrieved from the database (probably because they don't exist in the database). Submission should be
	 *      cancelled
	 */
	private RecordApometru[] getApometre() throws CommunicationException, SystemException
	{
		RecordFabricant[] fabs = dataHandler.getFabricanti();
		if (fabs == null)
		{
			fabs = new RecordFabricant[0];
		}

		RecordDN[] dns = dataHandler.getDNs();
		if (dns == null)
		{
			dns = new RecordDN[0];
		}

		int idDomPublic = dataHandler.getIdForDomeniuPublic();
		int idDomPrivat = dataHandler.getIdForDomeniuPrivat();

		if ((idDomPrivat == -1) || (idDomPublic == -1))
		{
			throw new SystemException(EMAErrorMessages.getInstance().getMessage("error.noDomeniu"));
		}

		RecordApometru[] result = new RecordApometru[wmTableModel.getRowCount()];

		for (int i = 0; i < wmTableModel.getRowCount(); i++)
		{
			int idDN = -1;
			boolean found = false;
			for (int j = 0; j < dns.length; j++)
			{
				if (dns[j].getNumar() == ((Integer) wmTableModel.getValueAt(i, 1)).intValue())
				{
					found = true;
					idDN = dns[j].getId();
				}
			}

			if (!found)
			{
				throw new SystemException(EMAErrorMessages.getInstance().getMessage("error.noDN"));
			}

			int idFabricant = -1;
			found = false;
			for (int j = 0; j < fabs.length; j++)
			{
				if (fabs[j].getNume().equals(wmTableModel.getValueAt(i, 2)))
				{
					found = true;
					idFabricant = fabs[j].getId();
				}
			}

			if (!found)
			{
				throw new SystemException(EMAErrorMessages.getInstance().getMessage("error.noFabricant"));
			}

			int idDomeniu = idDomPublic;
			if (((Boolean) wmTableModel.getValueAt(i, 5)).booleanValue() == true)
			{
				idDomeniu = idDomPrivat;
			}

			String serie = (String) wmTableModel.getValueAt(i, 3);
			String clasa = (String) wmTableModel.getValueAt(i, 4);
			String obs = (String) wmTableModel.getValueAt(i, 7);

			result[i] = new RecordApometru(-1, idFabricant, serie, idDN, clasa, idDomeniu, obs);
		}

		return result;
	}

	class ApometreTableModel extends AbstractTableModel
	{
		/**
		 * Creates a new ApometreTableModel with the row count intially set to <code>initialRowCount</code>.
		 *
		 * @param initialRowCount the initial row count
		 */
		public ApometreTableModel(int initialRowCount)
		{
			rowCount = initialRowCount;

			tableData = new ArrayList[NR_COLUMNS];
			for (int i = 0; i < NR_COLUMNS; i++)
			{
				tableData[i] = new ArrayList();
			}

			// initialize rows
			for (int i = 0; i < rowCount; i++)
			{
				setDefaultRowValues(i);
			}
		}


		/**
		 * Sets the values of all cells composing row <code>rowIndex</code> to their default values.
		 *
		 * @param rowIndex the row index identifying the row whose value to set
		 */
		public void setDefaultRowValues(int rowIndex)
		{
			for (int i = 0; i < NR_COLUMNS; i++)
			{
				switch (i)
				{
					case 0:
						setValueAt(new Integer(rowIndex + 1), rowIndex, i);
						break;
					case 1:
						setValueAt(defaultDNValue, rowIndex, i);
						break;
					case 2:
						setValueAt(defaultFabricantValue, rowIndex, i);
						break;
					case 3:
					case 4:
					case 7:
						setValueAt("", rowIndex, i);
						break;
					case 5:
					case 6:
						setValueAt(new Boolean(false), rowIndex, i);
						break;
				}
			}
		}

		public void addRow()
		{
			rowCount++;
			setDefaultRowValues(rowCount-1);
			fireTableRowsInserted(rowCount-1, rowCount-1);
		}

		public void deleteRow()
		{
			if (rowCount > 0)
			{
				rowCount--;
				fireTableRowsDeleted(rowCount, rowCount);
			}
		}

		public int getRowCount()
		{
			return rowCount;
		}

		public int getColumnCount()
		{
			return NR_COLUMNS;
		}

		public void setValueAt(Object aValue, int rowIndex, int columnIndex)
		{
			if (tableData[columnIndex].size() <= rowIndex)
			{
				tableData[columnIndex].add(rowIndex, aValue);
			}
			else
			{
				tableData[columnIndex].set(rowIndex, aValue);
			}
		}

		public Object getValueAt(int rowIndex, int columnIndex)
		{
			return tableData[columnIndex].get(rowIndex);
		}

		public String getColumnName(int column)
		{
			if ((column < 0) || (column >= NR_COLUMNS))
			{
				return "";
			}
			else
			{
				return columnNames[column];
			}
		}

		public Class getColumnClass(int columnIndex)
		{
			switch (columnIndex)
			{
				case 0:
				case 1:
					return Integer.class;
				case 2:
				case 3:
				case 4:
				case 7:
					return String.class;
				case 5:
				case 6:
					return Boolean.class;
				default:
					return Object.class;
			}
		}

		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			if (columnIndex == 0)
			{
				return false;
			}
			else
			{
				return true;
			}
		}

		private int rowCount;
		private ArrayList[] tableData;
	}

	class ButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() == addRowButton)
			{
				wmTableModel.addRow();
			}
			else if (e.getSource() == deleteRowButton)
			{
				wmTableModel.deleteRow();
			}
			else if (e.getSource() == saveButton)
			{
				try
				{
					if ((wmTable.getEditingRow() != -1) && (wmTable.getEditingColumn() != -1))
					{
						wmTable.getCellEditor(wmTable.getEditingRow(),
							wmTable.getEditingColumn()).stopCellEditing();
					}

					if (cerereData.submitCerere(CerereApometreScreen.this.getApometre()) == -1)
					{
						JOptionPane.showMessageDialog(parent,
							"Nu am reusit salvarea datelor. Verificati fisierul de log pentru detalii",
							EMAErrorMessages.getInstance().getMessage("error.name"),
							JOptionPane.ERROR_MESSAGE);
					}
					else
					{
						JOptionPane.showMessageDialog(parent,
							"Salvarea datelor s-a incheiat cu succes.",
							EMAErrorMessages.getInstance().getMessage("error.name"),
							JOptionPane.ERROR_MESSAGE);
					}
				}
				catch (CommunicationException ex)
				{
					JOptionPane.showMessageDialog(parent, ex.getMessage(),
						EMAErrorMessages.getInstance().getMessage("error.name"),
						JOptionPane.ERROR_MESSAGE);
				}
				catch (SystemException ex)
				{
					JOptionPane.showMessageDialog(parent, ex.getMessage(),
						EMAErrorMessages.getInstance().getMessage("error.name"),
						JOptionPane.ERROR_MESSAGE);
				}
			}
			else if (e.getSource() == addProducerButton)
			{
				// TODO: implement behaviour
			}
			else if (e.getSource() == addDnButton)
			{
				// TODO: implement behaviour
			}
		}
	}

	private final static int NR_COLUMNS = 8;

	/**
	 * A reference to the object handling all the businnes logic of this component.
	 */
	private CerereApometreData dataHandler;

	/**
	 * A reference to the data holder received from the calling screen (either CererePFScreen pr CererePJScreen).
	 */
	private Cerere cerereData;

	/**
	 * The parent frame.
	 */
	private JFrame parent;

	/**
	 * The array storing the table's column names.
	 */
	private String[] columnNames;

	/**
	 * The table containing the watermeter entries.
	 */
	private JTable wmTable;

	/**
	 * The model behind the wmTable.
	 */
	private ApometreTableModel wmTableModel;

	/**
	 * The buttons used in this screen.
	 */
	private JButton addRowButton;
	private JButton deleteRowButton;
	private JButton saveButton;
	private JButton addProducerButton;
	private JButton addDnButton;

	/**
	 * The default values for the Fabricant and DN combo boxes.
	 */
	private String defaultFabricantValue;
	private Integer defaultDNValue;
}
