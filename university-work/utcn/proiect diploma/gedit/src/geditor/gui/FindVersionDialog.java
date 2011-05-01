package geditor.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Date;
import java.util.Calendar;
import java.util.GregorianCalendar;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Mar 9, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class FindVersionDialog extends JDialog implements ActionListener
{
	public FindVersionDialog()
	{
		super(MainFrame.getInstance(), true);

		date = new GregorianCalendar();

		dateModel = new SpinnerDateModel(new Date(), null, null, Calendar.MINUTE);
		dateSpinner = new JSpinner(dateModel);

		okButton = new JButton("Ok");
		okButton.addActionListener(this);

		JPanel datePanel = new JPanel();
		JPanel buttonPanel = new JPanel();

		datePanel.add(new JLabel("The date & time of the version: "));
		datePanel.add(dateSpinner);

		buttonPanel.add(okButton);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(datePanel, BorderLayout.CENTER);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		setLocation((int) MainFrame.getInstance().getLocation().getX() + 100,
			(int) MainFrame.getInstance().getLocation().getY() + 100);
		pack();
	}

	public short getYear()
	{
		return (short) date.get(Calendar.YEAR);
	}

	public byte getMonth()
	{
		return (byte) (date.get(Calendar.MONTH) + 1);
	}

	public byte getDay()
	{
		return (byte) date.get(Calendar.DAY_OF_MONTH);
	}

	public byte getHour()
	{
		return (byte) date.get(Calendar.HOUR_OF_DAY);
	}

	public byte getMinute()
	{
		return (byte) date.get(Calendar.MINUTE);
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getSource() == okButton)
		{
			date = new GregorianCalendar();
			date.setTime(dateModel.getDate());
			dispose();
		}
	}

	private GregorianCalendar date;
	private JSpinner dateSpinner;
	private SpinnerDateModel dateModel;

	private JButton okButton;
}