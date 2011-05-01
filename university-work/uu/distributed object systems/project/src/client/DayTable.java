package client;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;

import core.Appointment;
import core.Period;

import java.util.List;

/**
 * 
 * JTable for showing the appointment for a day
 *
 * @author Richard Nieuwenhuis
 * @version 0.1
 * @date Apr 11, 2005
 */
public class DayTable extends AbstractTableModel{
	
	private String[] columnNames = {"Appointment",
								  "Begin time",
								  "End time",
								  "Description",
								  "Location",
								  "GroupAppointment?"};
	
	private Object[][] data;	
	
	public DayTable(List<Appointment> apps, ClientModel model)
	{
		int i = 0;
		data = new Object[apps.size()][6];
		for (Appointment app: apps)
		{
			data[i][0] = i+"";
			Period period = app.getTimeSlot();
			model.setToDate(period.getStartDate());
			data[i][1] = model.getDateRep();
			model.setToDate(period.getEndDate());
			data[i][2] = model.getDateRep();
			data[i][3] = app.getDescription();
			data[i][4] = app.getLocation();
			if (app.isGroupAppointment())
			{
				System.out.println("Jah is groep");
				data[i][5] = new Boolean(true);
			}
			else
				data[i][5] = new Boolean(false);
			i++;
		}
	}
	
    public int getColumnCount() {
        return columnNames.length;
    }

    public int getRowCount() {
        return data.length;
    }

    public String getColumnName(int col) {
        return columnNames[col];
    }

    public Object getValueAt(int row, int col) {
        return data[row][col];
    }

    public Class getColumnClass(int c) {
        return getValueAt(0, c).getClass();
    }

}
