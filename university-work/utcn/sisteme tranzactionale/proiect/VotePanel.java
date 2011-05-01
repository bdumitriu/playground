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
public class VotePanel extends JPanel
{
	public VotePanel()
	{
		movieCombo = new JComboBox();
		initMovieCombo();

		vote1 = new JComboBox();
		vote2 = new JComboBox();
		vote3 = new JComboBox();
		vote4 = new JComboBox();
		vote5 = new JComboBox();
		initVoteCombos();

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

		voteButton = new JButton("Submit vote(s)");
		voteButton.addActionListener(new MyActionListener());

		setLayout(new BorderLayout());

		JPanel northPanel = new JPanel();
		JPanel centerPanel = new JPanel();
		JPanel southPanel = new JPanel();

		add(northPanel, BorderLayout.NORTH);
		add(centerPanel, BorderLayout.CENTER);
		add(southPanel, BorderLayout.SOUTH);

		northPanel.add(movieCombo);
		southPanel.add(voteButton);

		centerPanel.setLayout(new GridLayout(1, 5));

		JPanel p1 = new JPanel();
		JPanel p2 = new JPanel();
		JPanel p3 = new JPanel();
		JPanel p4 = new JPanel();
		JPanel p5 = new JPanel();

		centerPanel.add(p1);
		centerPanel.add(p2);
		centerPanel.add(p3);
		centerPanel.add(p4);
		centerPanel.add(p5);

		p1.setLayout(new BorderLayout());
		p2.setLayout(new BorderLayout());
		p3.setLayout(new BorderLayout());
		p4.setLayout(new BorderLayout());
		p5.setLayout(new BorderLayout());

		JLabel voteLabel1 = new JLabel("The vote:");
		JLabel voteLabel2 = new JLabel("The vote:");
		JLabel voteLabel3 = new JLabel("The vote:");
		JLabel voteLabel4 = new JLabel("The vote:");
		JLabel voteLabel5 = new JLabel("The vote:");

		JLabel checkLabel1 = new JLabel("Include: ");
		JLabel checkLabel2 = new JLabel("Include: ");
		JLabel checkLabel3 = new JLabel("Include: ");
		JLabel checkLabel4 = new JLabel("Include: ");
		JLabel checkLabel5 = new JLabel("Include: ");

		JPanel p1South = new JPanel();
		JPanel p2South = new JPanel();
		JPanel p3South = new JPanel();
		JPanel p4South = new JPanel();
		JPanel p5South = new JPanel();

		p1South.add(checkLabel1);
		p1South.add(cb1);

		p2South.add(checkLabel2);
		p2South.add(cb2);

		p3South.add(checkLabel3);
		p3South.add(cb3);

		p4South.add(checkLabel4);
		p4South.add(cb4);

		p5South.add(checkLabel5);
		p5South.add(cb5);

		JPanel p1Center = new JPanel();
		JPanel p2Center = new JPanel();
		JPanel p3Center = new JPanel();
		JPanel p4Center = new JPanel();
		JPanel p5Center = new JPanel();

		p1Center.add(vote1);
		p2Center.add(vote2);
		p3Center.add(vote3);
		p4Center.add(vote4);
		p5Center.add(vote5);

		p1.add(voteLabel1, BorderLayout.NORTH);
		p2.add(voteLabel2, BorderLayout.NORTH);
		p3.add(voteLabel3, BorderLayout.NORTH);
		p4.add(voteLabel4, BorderLayout.NORTH);
		p5.add(voteLabel5, BorderLayout.NORTH);

		p1.add(p1Center, BorderLayout.CENTER);
		p2.add(p2Center, BorderLayout.CENTER);
		p3.add(p3Center, BorderLayout.CENTER);
		p4.add(p4Center, BorderLayout.CENTER);
		p5.add(p5Center, BorderLayout.CENTER);

		p1.add(p1South, BorderLayout.SOUTH);
		p2.add(p2South, BorderLayout.SOUTH);
		p3.add(p3South, BorderLayout.SOUTH);
		p4.add(p4South, BorderLayout.SOUTH);
		p5.add(p5South, BorderLayout.SOUTH);
	}

	private void initMovieCombo()
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		movies = new ArrayList();
		movieCombo.removeAllItems();

		try
		{
			PreparedStatement stmt = conn.prepareStatement("select id, title from Movie");

			ResultSet rs = stmt.executeQuery();

			while (rs.next())
			{
				movies.add(new MovieRecord(rs.getInt(1), rs.getString(2)));
				movieCombo.addItem(rs.getString(2));
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

	private void initVoteCombos()
	{
		initVoteCombo(vote1);
		initVoteCombo(vote2);
		initVoteCombo(vote3);
		initVoteCombo(vote4);
		initVoteCombo(vote5);
	}

	private void initVoteCombo(JComboBox voteCombo)
	{
		voteCombo.removeAllItems();
		for (int i = 1; i <= 10; i++)
		{
			voteCombo.addItem(new Integer(i));
		}
	}

	class MyActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() == voteButton)
			{
				MyThread[] threads = new MyThread[5];
				boolean[] active = new boolean[5];

				if (movies.size() == 0)
				{
					JOptionPane.showMessageDialog(VotePanel.this,
						"No movies are available in the database.", "Error",
						JOptionPane.ERROR_MESSAGE);
					return;
				}

				int idMovie = ((MovieRecord) movies.get(movieCombo.getSelectedIndex())).getId();

				if (cb1.isSelected())
				{
					threads[0] = new MyThread(idMovie, vote1.getSelectedIndex() + 1);
					active[0] = true;
				}
				else
				{
					active[0] = false;
				}

				if (cb2.isSelected())
				{
					threads[1] = new MyThread(idMovie, vote2.getSelectedIndex() + 1);
					active[1] = true;
				}
				else
				{
					active[1] = false;
				}

				if (cb3.isSelected())
				{
					threads[2] = new MyThread(idMovie, vote3.getSelectedIndex() + 1);
					active[2] = true;
				}
				else
				{
					active[2] = false;
				}

				if (cb4.isSelected())
				{
					threads[3] = new MyThread(idMovie, vote4.getSelectedIndex() + 1);
					active[3] = true;
				}
				else
				{
					active[3] = false;
				}

				if (cb5.isSelected())
				{
					threads[4] = new MyThread(idMovie, vote5.getSelectedIndex() + 1);
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
						message.append("Vote ");
						message.append(i+1);
						message.append(" submitted. The return code was: ");
						message.append(threads[i].getReturnCode());
						message.append(".\n");
					}
				}

				JOptionPane.showMessageDialog(VotePanel.this,
					message.toString(), "Info",
					JOptionPane.INFORMATION_MESSAGE);
			}
		}
	}

	class MyThread extends Thread
	{
		public MyThread(int idMovie, int vote)
		{
			this.idMovie = idMovie;
			this.vote = vote;
		}

		public void run()
		{
			Connection conn = DBInterfaceManager.getInstance().getConnection();

			try
			{
				CallableStatement stmt = conn.prepareCall("{?= call spRegisterVote(?,?)}");
				stmt.registerOutParameter(1, Types.INTEGER);
				stmt.setInt(2, idMovie);
				stmt.setInt(3, vote);

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
		private int vote;
		private int returnCode;
	}

	private JComboBox movieCombo;
	private ArrayList movies;

	private JButton voteButton;

	private JCheckBox cb1;
	private JCheckBox cb2;
	private JCheckBox cb3;
	private JCheckBox cb4;
	private JCheckBox cb5;

	private JComboBox vote1;
	private JComboBox vote2;
	private JComboBox vote3;
	private JComboBox vote4;
	private JComboBox vote5;
}