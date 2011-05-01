
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Observer;
import java.util.Observable;

/**
 * A GUI front end to the installer.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 */
public class GUIInstaller implements Observer
{
	public GUIInstaller(CLInstaller cli)
	{
		this.cli = cli;
	}

	public void install()
	{
		mainFrame = new JFrame("Church Ledger Installer");
		mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		initChooseLocation();

		mainFrame.setLocationByPlatform(true);
		mainFrame.setVisible(true);
		mainFrame.pack();
	}

	private void initChooseLocation()
	{
		String tmp = CLInstaller.config.getProperty("default.path");
		/*
		File initPath;
		boolean delete = false;
		if (tmp == null)
		{
			initPath = new File(System.getProperty("user.home"));
		}
		else
		{
			initPath = new File(tmp);
			if (!initPath.exists())
			{
				initPath.mkdirs();
				initPath.d
				delete = true;
			}
		}
		*/

		final JFileChooser jfc = new JFileChooser(tmp);
		jfc.setDialogTitle("Choose install location");
		jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		JLabel jl = new JLabel("Select install location: ");

		final JTextField jtf = new JTextField(tmp, 30);
		jtf.setEditable(false);

		JButton jb = new JButton("...");
		jb.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				if (jfc.showDialog(mainFrame, "Select") == JFileChooser.APPROVE_OPTION)
				{
					jtf.setText(jfc.getSelectedFile().getAbsolutePath());
				}
			}
		});

		JButton installButton = new JButton("Next >");
		installButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				cli.initWorkers(jtf.getText());
				initInstall();
			}
		});

		mainFrame.getContentPane().setLayout(new BorderLayout());

		JPanel locationPanel = new JPanel();
		locationPanel.add(jl);
		locationPanel.add(jtf);
		locationPanel.add(jb);

		JPanel buttonPanel = new JPanel();
		((FlowLayout) buttonPanel.getLayout()).setAlignment(FlowLayout.RIGHT);
		buttonPanel.add(installButton);

		mainFrame.getContentPane().add(locationPanel, BorderLayout.CENTER);
		mainFrame.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
	}

	private void initInstall()
	{
		Worker[] workers = cli.getWorkers();

		opBar = new ObserverProgressBar(0, 100, workers.length);
		opBar.setValue(0);
		opBar.setStringPainted(true);
		opBar.setPreferredSize(new Dimension(600, 25));

		for (int i = 0; i < workers.length; i++)
		{
			workers[i].addObserver(opBar);
		}

		messageLabel = new JLabel();

		finishButton = new JButton("Finish");
		finishButton.setEnabled(false);
		finishButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				close();
			}
		});

		JPanel barPanel = new JPanel();
		barPanel.add(opBar);

		JPanel messagePanel = new JPanel();
		messagePanel.add(messageLabel);

		JPanel buttonPanel = new JPanel();
		((FlowLayout) buttonPanel.getLayout()).setAlignment(FlowLayout.RIGHT);
		buttonPanel.add(finishButton);

		JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.add(messagePanel, BorderLayout.CENTER);
		southPanel.add(buttonPanel, BorderLayout.EAST);

		mainFrame.getContentPane().removeAll();
		mainFrame.getContentPane().add(barPanel, BorderLayout.CENTER);
		mainFrame.getContentPane().add(southPanel, BorderLayout.SOUTH);
		mainFrame.pack();
		mainFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

		WorkerThread th = new WorkerThread(workers);
		th.addObserver(this);
		Thread workerThread = new Thread(th);
		workerThread.start();

		Thread waitThread = new Thread(new WaitThread(workerThread, this));
		waitThread.start();
	}

	public void workerThreadEnded()
	{
		messageLabel.setText("Installation completed.");
		finishButton.setEnabled(true);
		mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	public void close()
	{
		mainFrame.dispose();
	}

	public void update(Observable o, Object arg)
	{
		messageLabel.setText(cli.getWorkers()[((Integer) arg).intValue()].getDescription());
	}

	private JFrame mainFrame;
	private JButton finishButton;
	private ObserverProgressBar opBar;
	private CLInstaller cli;
	private JLabel messageLabel;

	class WaitThread implements Runnable
	{
		public WaitThread(Thread th, GUIInstaller gui)
		{
			this.th = th;
			this.gui = gui;
		}

		public void run()
		{
			try
			{
				th.join();
			}
			catch (InterruptedException e)
			{}

			gui.workerThreadEnded();
		}

		private Thread th;
		private GUIInstaller gui;
	}
}
