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
public class AddMoviePanel extends JPanel
{
	public AddMoviePanel()
	{
		title = new JTextField(20);
		year = new JTextField(5);
		director = new JTextField(20);
		genre = new JTextField(20);
		runtime = new JTextField(5);
		language = new JTextField(20);
		insertButton = new JButton("Insert");

		insertButton.addActionListener(new MyActionListener());

		Box vBox = Box.createVerticalBox();

		add(vBox);

		Box hBox1 = Box.createHorizontalBox();
		Box hBox2 = Box.createHorizontalBox();
		Box hBox3 = Box.createHorizontalBox();
		Box hBox4 = Box.createHorizontalBox();
		Box hBox5 = Box.createHorizontalBox();
		Box hBox6 = Box.createHorizontalBox();
		Box hBox7 = Box.createHorizontalBox();

		vBox.add(hBox1);
		vBox.add(hBox2);
		vBox.add(hBox3);
		vBox.add(hBox4);
		vBox.add(hBox5);
		vBox.add(hBox6);
		vBox.add(hBox7);
		vBox.add(Box.createVerticalGlue());

	        hBox1.add(new JLabel("Title of the movie: "));
	        hBox1.add(title);

	        hBox2.add(new JLabel("Year movie was out in: "));
	        hBox2.add(year);

	        hBox3.add(new JLabel("Director(s) of the movie: "));
		hBox3.add(director);

		hBox4.add(new JLabel("Genre of the movie: "));
	        hBox4.add(genre);

	        hBox5.add(new JLabel("Total runtime: "));
	        hBox5.add(runtime);

	        hBox6.add(new JLabel("Language of the movie: "));
		hBox6.add(language);

		hBox7.add(insertButton);
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
					CallableStatement stmt = conn.prepareCall("{?= call spAddMovie(?,?,?,?,?,?)}");
					stmt.registerOutParameter(1, Types.INTEGER);
					stmt.setString(2, title.getText());
					stmt.setInt(3, (new Integer(year.getText())).intValue());
					stmt.setString(4, director.getText());
					stmt.setString(5, genre.getText());
					stmt.setInt(6, (new Integer(runtime.getText())).intValue());
					stmt.setString(7, language.getText());

					stmt.execute();

					int retVal = stmt.getInt(1);

					if (retVal != 0)
					{
						JOptionPane.showMessageDialog(AddMoviePanel.this,
							"The execution has failed with this error code: " + retVal +
							".", "Error", JOptionPane.ERROR_MESSAGE);
					}
					else
					{
						JOptionPane.showMessageDialog(AddMoviePanel.this,
							"Insertion was successful.", "Info",
							JOptionPane.INFORMATION_MESSAGE);
					}
				}
				catch (SQLException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(AddMoviePanel.this,
						"An SQL exception has occured.", "Error",
						JOptionPane.ERROR_MESSAGE);
				}
				catch (NumberFormatException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(AddMoviePanel.this,
						"The year and runtime need to be valid numbers.", "Error",
						JOptionPane.ERROR_MESSAGE);
				}
				finally
				{
					DBInterfaceManager.getInstance().close(conn);
				}
			}
		}
	}

	private JTextField title;
	private JTextField year;
	private JTextField director;
	private JTextField genre;
	private JTextField runtime;
	private JTextField language;

	private JButton insertButton;
}
