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
public class WeeklyTopTenPanel extends JPanel
{
	public WeeklyTopTenPanel()
	{
		setLayout(new BorderLayout());

		country = new JTextField(20);
		week = new JTextField(5);
		year = new JTextField(5);
		jsp = null;
		tfsPanel = new JPanel();
		refreshButton = new JButton("Display top ten");

		refreshButton.addActionListener(new MyActionListener());

		JPanel southPanel = new JPanel();
		southPanel.add(refreshButton);

		Box vBox = Box.createVerticalBox();

		tfsPanel.add(vBox);

		Box hBox1 = Box.createHorizontalBox();
		Box hBox2 = Box.createHorizontalBox();
		Box hBox3 = Box.createHorizontalBox();

		vBox.add(hBox1);
		vBox.add(hBox2);
		vBox.add(hBox3);
		vBox.add(Box.createVerticalGlue());

		hBox1.add(new JLabel("Country to show top ten for: "));
		hBox1.add(country);

		hBox2.add(new JLabel("Week to show top ten for: "));
		hBox2.add(week);

		hBox3.add(new JLabel("Year to show top ten for: "));
		hBox3.add(year);

		add(tfsPanel, BorderLayout.NORTH);
		add(southPanel, BorderLayout.SOUTH);
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
			JOptionPane.showMessageDialog(WeeklyTopTenPanel.this,
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
					PreparedStatement stmt = conn.prepareStatement("select id from Country where country = ?");
					stmt.setString(1, country.getText());

					ResultSet rs = stmt.executeQuery();

					int idCountry = -1;
					if (rs.next())
					{
						idCountry = rs.getInt(1);
					}
					else
					{
						JOptionPane.showMessageDialog(WeeklyTopTenPanel.this,
							"No such country exists in the database.", "Info",
							JOptionPane.INFORMATION_MESSAGE);
						DBInterfaceManager.getInstance().close(conn);
						return;
					}

					CallableStatement cStmt = conn.prepareCall("{?= call spGetWeeklyTopTen(?,?,?)}");
					cStmt.registerOutParameter(1, Types.INTEGER);
					cStmt.setInt(2, idCountry);
					cStmt.setInt(3, (new Integer(week.getText())).intValue());
					cStmt.setInt(4, (new Integer(year.getText())).intValue());

					rs = cStmt.executeQuery();
					showInTable(rs);

					int retVal = cStmt.getInt(1);

					if (retVal != 0)
					{
						JOptionPane.showMessageDialog(WeeklyTopTenPanel.this,
							"The execution has failed with this error code: " + retVal +
							".", "Error", JOptionPane.ERROR_MESSAGE);
					}
				}
				catch (SQLException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(WeeklyTopTenPanel.this,
						"An SQL exception has occured.", "Error",
						JOptionPane.ERROR_MESSAGE);
				}
				catch (NumberFormatException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(WeeklyTopTenPanel.this,
						"The week and year need to be valid numbers.", "Error",
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

	private JTextField country;
	private JTextField week;
	private JTextField year;

	private JTable table;

	private JScrollPane jsp;

	private JButton refreshButton;

	private JPanel tfsPanel;
}