import javax.swing.*;
import javax.swing.table.TableModel;
import javax.swing.table.DefaultTableModel;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;
import java.sql.*;
import java.util.ArrayList;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 3, 2004
 */
public class TopTenPanel extends JPanel
{
	public TopTenPanel()
	{
		setLayout(new BorderLayout());

		jsp = null;
		refreshButton = new JButton("Refresh top ten");

		refreshButton.addActionListener(new MyActionListener());

		JPanel southPanel = new JPanel();
		southPanel.add(refreshButton);

		add(southPanel, BorderLayout.SOUTH);

		refreshButton.doClick();
	}

	private void showInTable(ResultSet rs)
	{
		ArrayList data = new ArrayList();

		try
		{
			while (rs.next())
			{
				data.add(new GrossRecord(rs.getString(1), rs.getString(2), rs.getInt(3)));
			}
		}
		catch (SQLException e)
		{
			e.printStackTrace();
			JOptionPane.showMessageDialog(TopTenPanel.this,
				"An SQL exception has occured.", "Error",
				JOptionPane.ERROR_MESSAGE);
			return;
		}

		table = new JTable(new MyTableModel(data.size(), 3));

		TableModel model = table.getModel();
		for (int i = 0; i < data.size(); i++)
		{
			GrossRecord rec = (GrossRecord) data.get(i);

			model.setValueAt(rec.getTitle(), i, 0);
			model.setValueAt(rec.getDirector(), i, 1);
			model.setValueAt(new Integer(rec.getTotalGross()), i, 2);
		}

		if (jsp != null)
		{
			remove(jsp);
		}

		jsp = new JScrollPane(table);
		add(jsp, BorderLayout.CENTER);

		validate();
	}

	class MyActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() == refreshButton)
			{
				Connection conn = DBInterfaceManager.getInstance().getConnection();

				try
				{
					CallableStatement stmt = conn.prepareCall("{?= call spGetTopTen()}");
					stmt.registerOutParameter(1, Types.INTEGER);

					ResultSet rs = stmt.executeQuery();
					showInTable(rs);

					int retVal = stmt.getInt(1);

					if (retVal != 0)
					{
						JOptionPane.showMessageDialog(TopTenPanel.this,
							"The execution has failed with this error code: " + retVal +
							".", "Error", JOptionPane.ERROR_MESSAGE);
					}
				}
				catch (SQLException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(TopTenPanel.this,
						"An SQL exception has occured.", "Error",
						JOptionPane.ERROR_MESSAGE);
				}
				finally
				{
					DBInterfaceManager.getInstance().close(conn);
				}
			}
		}
	}

	class MyTableModel extends DefaultTableModel
	{
		public MyTableModel(int rowCount, int columnCount)
		{
			super(rowCount, columnCount);
		}

		public Class getColumnClass(int columnIndex)
		{
			switch (columnIndex)
			{
				case 0:
				case 1:
					return String.class;
				case 2:
					return Integer.class;
				default:
					return Object.class;
			}
		}

		public String getColumnName(int column)
		{
			switch (column)
			{
				case 0:
					return "Movie title";
				case 1:
					return "Movie director";
				case 2:
					return "Total gross";
				default:
					return "";
			}
		}

		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			return false;
		}
	}

	private JTable table;

	private JScrollPane jsp;

	private JButton refreshButton;
}