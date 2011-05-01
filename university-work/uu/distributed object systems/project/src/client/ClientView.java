package client;

import javax.swing.*;
import java.awt.*;
import java.util.Observable;
import java.util.Observer;
public class ClientView implements Observer {
	
	private ClientModel model;
	private JTable table;
	public ClientView(ClientModel model)
	{
		this.model = model;
		model.addObserver(this);
	}
	
	public void update(Observable o, Object arg)
	{
		createAppointmentTable();
	}
	
	private void createAppointmentTable()
	{		
	}
	
	public void paint(Graphics g)
	{
	}

}