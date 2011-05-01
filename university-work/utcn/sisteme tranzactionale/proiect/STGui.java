import javax.swing.*;

/**
 * The GUI for the ST Project.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 3, 2004
 */
public class STGui extends JFrame
{
	public static void main(String[] args)
	{
		if ((args.length != 2) && (args.length != 3))
		{
			System.out.println("Usage: java STGui db-server db-user [db-password]");
			System.out.println(" -- if db-password is missing, the empty string will be used");
			System.exit(1);
		}
		else if (args.length == 2)
		{
			DBInterfaceManager.getInstance(args[0], args[1], "");
		}
		else
		{
			DBInterfaceManager.getInstance(args[0], args[1], args[2]);
		}

		new STGui();
	}

	public STGui()
	{
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setFramePositionOnScreen();
		addComponents();
		setTitle("Proiect Sisteme Tranzactionale");
		show();
	}

	/**
	 * Adds all the components to the frame.
	 */
	private void addComponents()
	{
		jtp = new JTabbedPane(JTabbedPane.BOTTOM);

		addCinemaPanel = new AddCinemaPanel();
		addMoviePanel = new AddMoviePanel();
		votePanel = new VotePanel();
		incomePanel = new IncomePanel();
		showVotePanel = new ShowVotePanel();
		weeklyTopTenPanel = new WeeklyTopTenPanel();
		topTenPanel = new TopTenPanel();

		jtp.addTab("Add cinema", addCinemaPanel);
		jtp.addTab("Add movie", addMoviePanel);
		jtp.addTab("Vote movie", votePanel);
		jtp.addTab("Report income", incomePanel);
		jtp.addTab("Movie votes", showVotePanel);
		jtp.addTab("Weekly top ten", weeklyTopTenPanel);
		jtp.addTab("All-time top ten", topTenPanel);

		getContentPane().add(jtp);
	}

	/**
	 * Decide where to put the frame and how big to make it.
	 */
	private void setFramePositionOnScreen()
	{
		//Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		//int height = (int) screenSize.getHeight() - 200;
		//int width = (int) screenSize.getWidth() - 200;
		int height = 250;
		int width = 800;
		setBounds(100, 200, width, height);
	}

	private JTabbedPane jtp;

	private AddCinemaPanel addCinemaPanel;
	private AddMoviePanel addMoviePanel;
	private VotePanel votePanel;
	private IncomePanel incomePanel;
	private ShowVotePanel showVotePanel;
	private WeeklyTopTenPanel weeklyTopTenPanel;
	private TopTenPanel topTenPanel;
}