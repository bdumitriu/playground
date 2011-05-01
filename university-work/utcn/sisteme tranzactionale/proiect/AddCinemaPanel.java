import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.CallableStatement;
import java.sql.Types;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 3, 2004
 */
public class AddCinemaPanel extends JPanel
{
	public AddCinemaPanel()
	{
		name = new JTextField(20);
		country = new JTextField(20);
		capacity = new JTextField(5);
		insertButton = new JButton("Insert");

		insertButton.addActionListener(new MyActionListener());

		Box vBox = Box.createVerticalBox();

		add(vBox);

		Box hBox1 = Box.createHorizontalBox();
		Box hBox2 = Box.createHorizontalBox();
		Box hBox3 = Box.createHorizontalBox();
		Box hBox4 = Box.createHorizontalBox();

		vBox.add(hBox1);
		vBox.add(hBox2);
		vBox.add(hBox3);
		vBox.add(hBox4);
		vBox.add(Box.createVerticalGlue());

	        hBox1.add(new JLabel("Name of the cinema: "));
	        hBox1.add(name);

	        hBox2.add(new JLabel("Country of the cinema: "));
	        hBox2.add(country);

	        hBox3.add(new JLabel("Capacity of the cinema: "));
		hBox3.add(capacity);

		hBox4.add(insertButton);
	}

	class MyActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() == insertButton)
			{
				Connection conn = DBInterfaceManager.getInstance().getConnection();

				try
				{
					CallableStatement stmt = conn.prepareCall("{?= call spAddCinema(?,?,?)}");
					stmt.registerOutParameter(1, Types.INTEGER);
					stmt.setString(2, name.getText());
					stmt.setString(3, country.getText());
					stmt.setInt(4, (new Integer(capacity.getText())).intValue());

					stmt.execute();

					int retVal = stmt.getInt(1);

					if (retVal != 0)
					{
						JOptionPane.showMessageDialog(AddCinemaPanel.this,
							"The execution has failed with this error code: " + retVal +
							".", "Error", JOptionPane.ERROR_MESSAGE);
					}
					else
					{
						JOptionPane.showMessageDialog(AddCinemaPanel.this,
							"Insertion was successful", "Info",
							JOptionPane.INFORMATION_MESSAGE);
					}
				}
				catch (SQLException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(AddCinemaPanel.this,
						"An SQL exception has occured.", "Error",
						JOptionPane.ERROR_MESSAGE);
				}
				catch (NumberFormatException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(AddCinemaPanel.this,
						"The capacity needs to be a valid number.", "Error",
						JOptionPane.ERROR_MESSAGE);
				}
				finally
				{
					DBInterfaceManager.getInstance().close(conn);
				}
			}
		}
	}

	private JTextField name;
	private JTextField country;
	private JTextField capacity;

	private JButton insertButton;
}