
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 *
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 * 
 * Date: May 8, 2003
 */
public class DNS extends JFrame implements ActionListener
{
	public static void main(String[] args)
	{
		DNS dns = new DNS();

		dns.setLocation(300, 400);
		dns.pack();
		dns.show();
	}

	public DNS()
	{
		addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				System.exit(0);
			}
		});

		getContentPane().setLayout(new GridLayout(2, 1));
		getContentPane().add(upPanel);
		getContentPane().add(downPanel);

		upPanel.setLayout(new GridLayout(2, 1));
		upPanel.add(addrPan);
		upPanel.add(ipPan);
		addrPan.add(new JLabel("Address: "));
		addressTF.setColumns(25);
		addrPan.add(addressTF);
		ipPan.add(new JLabel("IP: "));
		ipTF.setEditable(false);
		ipTF.setColumns(25);
		ipPan.add(ipTF);

		downPanel.setLayout(new FlowLayout());
		downPanel.add(transB);
		downPanel.add(exitB);

		transB.addActionListener(this);
		exitB.addActionListener(this);
		addressTF.addKeyListener(new KeyAdapter()
		{
			public void keyPressed(KeyEvent e)
			{
				if (e.getKeyCode() == KeyEvent.VK_ENTER)
				{
					transB.doClick();
				}
			}
		});
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getSource().equals(transB))
		{
			try
			{
				inetAddr = InetAddress.getByName(addressTF.getText());
				ipTF.setText(inetAddr.getHostAddress());
			}
			catch (UnknownHostException e1)
			{
				ipTF.setText("Not found.");
			}
		}
		else if (e.getSource().equals(exitB))
		{
			System.exit(0);
		}
	}

	JPanel upPanel = new JPanel();
	JPanel downPanel = new JPanel();
	JPanel addrPan = new JPanel();
	JPanel ipPan = new JPanel();

	JTextField addressTF = new JTextField();
	JTextField ipTF = new JTextField();

	JButton transB = new JButton("translate");
	JButton exitB = new JButton("exit");

	InetAddress inetAddr;
}
