import javax.swing.*;
import java.sql.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 4, 2004
 */
public class IncomePanel extends JPanel
{
	public IncomePanel()
	{
		movieCombo1 = new JComboBox();
		movieCombo2 = new JComboBox();
		movieCombo3 = new JComboBox();
		movieCombo4 = new JComboBox();
		movieCombo5 = new JComboBox();
		initMovieCombos();

		cinemaCombo1 = new JComboBox();
		cinemaCombo2 = new JComboBox();
		cinemaCombo3 = new JComboBox();
		cinemaCombo4 = new JComboBox();
		cinemaCombo5 = new JComboBox();
		initCinemaCombos();

		week1 = new JTextField(5);
		week2 = new JTextField(5);
		week3 = new JTextField(5);
		week4 = new JTextField(5);
		week5 = new JTextField(5);

		year1 = new JTextField(5);
		year2 = new JTextField(5);
		year3 = new JTextField(5);
		year4 = new JTextField(5);
		year5 = new JTextField(5);

		gross1 = new JTextField(10);
		gross2 = new JTextField(10);
		gross3 = new JTextField(10);
		gross4 = new JTextField(10);
		gross5 = new JTextField(10);

//		JLabel movieLabel1 = new JLabel("Movie:");
//		JLabel movieLabel2 = new JLabel("Movie:");
//		JLabel movieLabel3 = new JLabel("Movie:");
//		JLabel movieLabel4 = new JLabel("Movie:");
//		JLabel movieLabel5 = new JLabel("Movie:");
//
//		JLabel cinemaLabel1 = new JLabel("Cinema:");
//		JLabel cinemaLabel2 = new JLabel("Cinema:");
//		JLabel cinemaLabel3 = new JLabel("Cinema:");
//		JLabel cinemaLabel4 = new JLabel("Cinema:");
//		JLabel cinemaLabel5 = new JLabel("Cinema:");

		JLabel weekLabel1 = new JLabel("Week:");
		JLabel weekLabel2 = new JLabel("Week:");
		JLabel weekLabel3 = new JLabel("Week:");
		JLabel weekLabel4 = new JLabel("Week:");
		JLabel weekLabel5 = new JLabel("Week:");

		JLabel yearLabel1 = new JLabel("Year:");
		JLabel yearLabel2 = new JLabel("Year:");
		JLabel yearLabel3 = new JLabel("Year:");
		JLabel yearLabel4 = new JLabel("Year:");
		JLabel yearLabel5 = new JLabel("Year:");

		JLabel grossLabel1 = new JLabel("Income:");
		JLabel grossLabel2 = new JLabel("Income:");
		JLabel grossLabel3 = new JLabel("Income:");
		JLabel grossLabel4 = new JLabel("Income:");
		JLabel grossLabel5 = new JLabel("Income:");

		JLabel checkLabel1 = new JLabel("Include: ");
		JLabel checkLabel2 = new JLabel("Include: ");
		JLabel checkLabel3 = new JLabel("Include: ");
		JLabel checkLabel4 = new JLabel("Include: ");
		JLabel checkLabel5 = new JLabel("Include: ");

		cb1 = new JCheckBox();
		cb2 = new JCheckBox();
		cb3 = new JCheckBox();
		cb4 = new JCheckBox();
		cb5 = new JCheckBox();

		cb1.setSelected(true);
		cb2.setSelected(true);
		cb3.setSelected(true);
		cb4.setSelected(true);
		cb5.setSelected(true);

		submitButton = new JButton("Submit income");
		submitButton.addActionListener(new MyActionListener());

		setLayout(new BorderLayout());

		JPanel centerPanel = new JPanel();
		JPanel southPanel = new JPanel();

		add(centerPanel, BorderLayout.CENTER);
		add(southPanel, BorderLayout.SOUTH);

		southPanel.add(submitButton);

		centerPanel.setLayout(new GridLayout(1, 5));

		Box vb1 = Box.createVerticalBox();
		Box vb2 = Box.createVerticalBox();
		Box vb3 = Box.createVerticalBox();
		Box vb4 = Box.createVerticalBox();
		Box vb5 = Box.createVerticalBox();

		centerPanel.add(vb1);
		centerPanel.add(vb2);
		centerPanel.add(vb3);
		centerPanel.add(vb4);
		centerPanel.add(vb5);

		Box hb11 = Box.createHorizontalBox();
		Box hb12 = Box.createHorizontalBox();
		Box hb13 = Box.createHorizontalBox();
		Box hb14 = Box.createHorizontalBox();
		Box hb15 = Box.createHorizontalBox();
		Box hb16 = Box.createHorizontalBox();

		vb1.add(hb11);
		vb1.add(hb12);
		vb1.add(hb13);
		vb1.add(hb14);
		vb1.add(hb15);
		vb1.add(hb16);
		vb1.add(Box.createVerticalGlue());

		Box hb21 = Box.createHorizontalBox();
		Box hb22 = Box.createHorizontalBox();
		Box hb23 = Box.createHorizontalBox();
		Box hb24 = Box.createHorizontalBox();
		Box hb25 = Box.createHorizontalBox();
		Box hb26 = Box.createHorizontalBox();

		vb2.add(hb21);
		vb2.add(hb22);
		vb2.add(hb23);
		vb2.add(hb24);
		vb2.add(hb25);
		vb2.add(hb26);
		vb2.add(Box.createVerticalGlue());

		Box hb31 = Box.createHorizontalBox();
		Box hb32 = Box.createHorizontalBox();
		Box hb33 = Box.createHorizontalBox();
		Box hb34 = Box.createHorizontalBox();
		Box hb35 = Box.createHorizontalBox();
		Box hb36 = Box.createHorizontalBox();

		vb3.add(hb31);
		vb3.add(hb32);
		vb3.add(hb33);
		vb3.add(hb34);
		vb3.add(hb35);
		vb3.add(hb36);
		vb3.add(Box.createVerticalGlue());

		Box hb41 = Box.createHorizontalBox();
		Box hb42 = Box.createHorizontalBox();
		Box hb43 = Box.createHorizontalBox();
		Box hb44 = Box.createHorizontalBox();
		Box hb45 = Box.createHorizontalBox();
		Box hb46 = Box.createHorizontalBox();

		vb4.add(hb41);
		vb4.add(hb42);
		vb4.add(hb43);
		vb4.add(hb44);
		vb4.add(hb45);
		vb4.add(hb46);
		vb4.add(Box.createVerticalGlue());

		Box hb51 = Box.createHorizontalBox();
		Box hb52 = Box.createHorizontalBox();
		Box hb53 = Box.createHorizontalBox();
		Box hb54 = Box.createHorizontalBox();
		Box hb55 = Box.createHorizontalBox();
		Box hb56 = Box.createHorizontalBox();

		vb5.add(hb51);
		vb5.add(hb52);
		vb5.add(hb53);
		vb5.add(hb54);
		vb5.add(hb55);
		vb5.add(hb56);
		vb5.add(Box.createVerticalGlue());

		//hb11.add(movieLabel1);
		hb11.add(movieCombo1);

		//hb21.add(movieLabel2);
		hb21.add(movieCombo2);

		//hb31.add(movieLabel3);
		hb31.add(movieCombo3);

		//hb41.add(movieLabel4);
		hb41.add(movieCombo4);

		//hb51.add(movieLabel5);
		hb51.add(movieCombo5);

		//hb12.add(cinemaLabel1);
		hb12.add(cinemaCombo1);

		//hb22.add(cinemaLabel2);
		hb22.add(cinemaCombo2);

		//hb32.add(cinemaLabel3);
		hb32.add(cinemaCombo3);

		//hb42.add(cinemaLabel4);
		hb42.add(cinemaCombo4);

		//hb52.add(cinemaLabel5);
		hb52.add(cinemaCombo5);

		hb13.add(weekLabel1);
		hb13.add(week1);

		hb23.add(weekLabel2);
		hb23.add(week2);

		hb33.add(weekLabel3);
		hb33.add(week3);

		hb43.add(weekLabel4);
		hb43.add(week4);

		hb53.add(weekLabel5);
		hb53.add(week5);

		hb14.add(yearLabel1);
		hb14.add(year1);

		hb24.add(yearLabel2);
		hb24.add(year2);

		hb34.add(yearLabel3);
		hb34.add(year3);

		hb44.add(yearLabel4);
		hb44.add(year4);

		hb54.add(yearLabel5);
		hb54.add(year5);

		hb15.add(grossLabel1);
		hb15.add(gross1);

		hb25.add(grossLabel2);
		hb25.add(gross2);

		hb35.add(grossLabel3);
		hb35.add(gross3);

		hb45.add(grossLabel4);
		hb45.add(gross4);

		hb55.add(grossLabel5);
		hb55.add(gross5);

		hb16.add(checkLabel1);
		hb16.add(cb1);

		hb26.add(checkLabel2);
		hb26.add(cb2);

		hb36.add(checkLabel3);
		hb36.add(cb3);

		hb46.add(checkLabel4);
		hb46.add(cb4);

		hb56.add(checkLabel5);
		hb56.add(cb5);
	}

	private void initMovieCombos()
	{
		readMovies();
		initMovieCombo(movieCombo1);
		initMovieCombo(movieCombo2);
		initMovieCombo(movieCombo3);
		initMovieCombo(movieCombo4);
		initMovieCombo(movieCombo5);
	}

	private void initMovieCombo(JComboBox movieCombo)
	{
		movieCombo.removeAllItems();
		for (int i = 0; i < movies.size(); i++)
		{
			movieCombo.addItem(((MovieRecord) movies.get(i)).getTitle());
		}
	}

	private void readMovies()
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		movies = new ArrayList();

		try
		{
			PreparedStatement stmt = conn.prepareStatement("select id, title from Movie");

			ResultSet rs = stmt.executeQuery();

			while (rs.next())
			{
				movies.add(new MovieRecord(rs.getInt(1), rs.getString(2)));
			}
		}
		catch (SQLException e)
		{
			e.printStackTrace();
			JOptionPane.showMessageDialog(this,
				"An SQL exception has occured.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		finally
		{
			DBInterfaceManager.getInstance().close(conn);
		}
	}

	private void initCinemaCombos()
	{
		readCinemas();
		initCinemaCombo(cinemaCombo1);
		initCinemaCombo(cinemaCombo2);
		initCinemaCombo(cinemaCombo3);
		initCinemaCombo(cinemaCombo4);
		initCinemaCombo(cinemaCombo5);
	}

	private void initCinemaCombo(JComboBox cinemaCombo)
	{
		cinemaCombo.removeAllItems();
		for (int i = 0; i < cinemas.size(); i++)
		{
			cinemaCombo.addItem(((CinemaRecord) cinemas.get(i)).getName());
		}
	}

	private void readCinemas()
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		cinemas = new ArrayList();

		try
		{
			PreparedStatement stmt = conn.prepareStatement("select id, name from Cinema");

			ResultSet rs = stmt.executeQuery();

			while (rs.next())
			{
				cinemas.add(new CinemaRecord(rs.getInt(1), rs.getString(2)));
			}
		}
		catch (SQLException e)
		{
			e.printStackTrace();
			JOptionPane.showMessageDialog(this,
				"An SQL exception has occured.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		finally
		{
			DBInterfaceManager.getInstance().close(conn);
		}
	}


	class MyActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() == submitButton)
			{
				try
				{
					MyThread[] threads = new MyThread[5];
					boolean[] active = new boolean[5];

					if (movies.size() == 0)
					{
						JOptionPane.showMessageDialog(IncomePanel.this,
							"No movies are available in the database.", "Error",
							JOptionPane.ERROR_MESSAGE);
						return;
					}

					if (cb1.isSelected())
					{
						int idMovie = ((MovieRecord) movies.get(movieCombo1.getSelectedIndex())).getId();
						int idCinema = ((CinemaRecord) cinemas.get(cinemaCombo1.getSelectedIndex())).getId();
						int week = (new Integer(week1.getText())).intValue();
						int year = (new Integer(year1.getText())).intValue();
						int gross = (new Integer(gross1.getText())).intValue();

						threads[0] = new MyThread(idMovie, idCinema, week, year, gross);
						active[0] = true;
					}
					else
					{
						active[0] = false;
					}

					if (cb2.isSelected())
					{
						int idMovie = ((MovieRecord) movies.get(movieCombo2.getSelectedIndex())).getId();
						int idCinema = ((CinemaRecord) cinemas.get(cinemaCombo2.getSelectedIndex())).getId();
						int week = (new Integer(week2.getText())).intValue();
						int year = (new Integer(year2.getText())).intValue();
						int gross = (new Integer(gross2.getText())).intValue();

						threads[1] = new MyThread(idMovie, idCinema, week, year, gross);
						active[1] = true;
					}
					else
					{
						active[1] = false;
					}

					if (cb3.isSelected())
					{
						int idMovie = ((MovieRecord) movies.get(movieCombo3.getSelectedIndex())).getId();
						int idCinema = ((CinemaRecord) cinemas.get(cinemaCombo3.getSelectedIndex())).getId();
						int week = (new Integer(week3.getText())).intValue();
						int year = (new Integer(year3.getText())).intValue();
						int gross = (new Integer(gross3.getText())).intValue();

						threads[2] = new MyThread(idMovie, idCinema, week, year, gross);
						active[2] = true;
					}
					else
					{
						active[2] = false;
					}

					if (cb4.isSelected())
					{
						int idMovie = ((MovieRecord) movies.get(movieCombo4.getSelectedIndex())).getId();
						int idCinema = ((CinemaRecord) cinemas.get(cinemaCombo4.getSelectedIndex())).getId();
						int week = (new Integer(week4.getText())).intValue();
						int year = (new Integer(year4.getText())).intValue();
						int gross = (new Integer(gross4.getText())).intValue();

						threads[3] = new MyThread(idMovie, idCinema, week, year, gross);
						active[3] = true;
					}
					else
					{
						active[3] = false;
					}

					if (cb5.isSelected())
					{
						int idMovie = ((MovieRecord) movies.get(movieCombo5.getSelectedIndex())).getId();
						int idCinema = ((CinemaRecord) cinemas.get(cinemaCombo5.getSelectedIndex())).getId();
						int week = (new Integer(week5.getText())).intValue();
						int year = (new Integer(year5.getText())).intValue();
						int gross = (new Integer(gross5.getText())).intValue();

						threads[4] = new MyThread(idMovie, idCinema, week, year, gross);
						active[4] = true;
					}
					else
					{
						active[4] = false;
					}

					for (int i = 0; i < 5; i++)
					{
						if (active[i] == true)
						{
							threads[i].start();
						}
					}

					for (int i = 0; i < 5; i++)
					{
						if (active[i] == true)
						{
							try
							{
								threads[i].join();
							}
							catch (InterruptedException ex)
							{}
						}
					}

					StringBuffer message = new StringBuffer("Here is the outcome:\n\n");
					for (int i = 0; i < 5; i++)
					{
						if (active[i] == true)
						{
							message.append("Income ");
							message.append(i+1);
							message.append(" submitted. The return code was: ");
							message.append(threads[i].getReturnCode());
							message.append(".\n");
						}
					}

					JOptionPane.showMessageDialog(IncomePanel.this,
						message.toString(), "Info",
						JOptionPane.INFORMATION_MESSAGE);
				}
				catch (NumberFormatException ex)
				{
					ex.printStackTrace();
					JOptionPane.showMessageDialog(IncomePanel.this,
						"All weeks, years and incomes need to be valid numbers.", "Error",
						JOptionPane.ERROR_MESSAGE);
				}
			}
		}
	}

	class MyThread extends Thread
	{
		public MyThread(int idMovie, int idCinema, int week, int year, int gross)
		{
			this.idMovie = idMovie;
			this.idCinema = idCinema;
			this.week = week;
			this.year = year;
			this.gross = gross;
		}

		public void run()
		{
			Connection conn = DBInterfaceManager.getInstance().getConnection();

			try
			{
				CallableStatement stmt = conn.prepareCall("{?= call spAddIncome(?,?,?,?,?)}");
				stmt.registerOutParameter(1, Types.INTEGER);
				stmt.setInt(2, idCinema);
				stmt.setInt(3, idMovie);
				stmt.setInt(4, week);
				stmt.setInt(5, year);
				stmt.setInt(6, gross);


				stmt.execute();

				returnCode = stmt.getInt(1);
			}
			catch (SQLException ex)
			{
				ex.printStackTrace();
				returnCode = -1;
			}
			finally
			{
				DBInterfaceManager.getInstance().close(conn);
			}
		}

		public int getReturnCode()
		{
			return returnCode;
		}

		private int idMovie;
		private int idCinema;
		private int week;
		private int year;
		private int gross;
		private int returnCode;
	}

	private JComboBox movieCombo1;
	private JComboBox movieCombo2;
	private JComboBox movieCombo3;
	private JComboBox movieCombo4;
	private JComboBox movieCombo5;

	private JComboBox cinemaCombo1;
	private JComboBox cinemaCombo2;
	private JComboBox cinemaCombo3;
	private JComboBox cinemaCombo4;
	private JComboBox cinemaCombo5;

	private JTextField week1;
	private JTextField week2;
	private JTextField week3;
	private JTextField week4;
	private JTextField week5;

	private JTextField year1;
	private JTextField year2;
	private JTextField year3;
	private JTextField year4;
	private JTextField year5;

	private JTextField gross1;
	private JTextField gross2;
	private JTextField gross3;
	private JTextField gross4;
	private JTextField gross5;

	private JCheckBox cb1;
	private JCheckBox cb2;
	private JCheckBox cb3;
	private JCheckBox cb4;
	private JCheckBox cb5;

	private ArrayList movies;
	private ArrayList cinemas;

	private JButton submitButton;
}