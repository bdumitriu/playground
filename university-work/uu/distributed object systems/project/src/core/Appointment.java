package core;

import java.sql.*;
import java.io.Serializable;

import core.exceptions.*;

/**
 * A simple class to represent appointemnts
 *.
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 10, 2005
 */
public class Appointment implements DomainObject, Serializable
{
	public Appointment()
	{}

	public int getId()
	{
		return id;
	}

	public void setId(int id)
	{
		this.id = id;
	}

	/**
	 * Gets a fresh id from the database and sets it as the id of this appointment.
	 *
	 * @param conn a valid connection to the database.
	 * @throws SQLException if an SQLException occurs while communicating to the database, it is forwarded to the
	 *	caller.
	 */
	public void setIdFromDb(Connection conn) throws SQLException
	{
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(nextValStatement());
		if (rs.next())
		{
			id = rs.getInt(1);
		}
	}

	public Period getTimeSlot()
	{
		return timeSlot;
	}

	public void setTimeSlot(Period timeSlot)
	{
		this.timeSlot = timeSlot;
	}

	public String getLocation()
	{
		return location;
	}

	public void setLocation(String location)
	{
		this.location = location;
	}

	public String getDescription()
	{
		return description;
	}

	public void setDescription(String description)
	{
		this.description = description;
	}

	public boolean isGroupAppointment()
	{
		return isGroupAppointment;
	}

	public void setGroupAppointment(boolean groupAppointment)
	{
		isGroupAppointment = groupAppointment;
	}

	public String toString()
	{
		StringBuilder sb = new StringBuilder("[id: ");
		sb.append(id);
		sb.append(", begins at: ");
		sb.append(timeSlot.getStartDate());
		sb.append(", ends at: ");
		sb.append(timeSlot.getEndDate());
		sb.append(", description: ");
		sb.append(description);
		sb.append(", location: ");
		sb.append(location);
		sb.append(", is group appointment: ");
		sb.append(isGroupAppointment);
		sb.append("]");

		return sb.toString();
	}

	/**
	 * Insert this appointment as a new appointment in the database. Make sure you use the
	 * {@link #setIdFromDb(java.sql.Connection)} method before calling this one in order to generate a new unique
	 * id for the appointment object. Otherwise, chances are that the insertion will fail due to duplicate
	 * appoinetment id's in the database.
	 *
	 * @param conn a valid connection to the database.
	 * @throws SQLException if an SQLException occurs while communicating to the database, it is forwarded to the
	 *	caller.
	 */
	public void dbInsert(Connection conn) throws SQLException
	{
		PreparedStatement insertStatement = conn.prepareStatement(insertAppointmentStatement());
		insertStatement.setInt(1, id);
		insertStatement.setTimestamp(2, new java.sql.Timestamp(timeSlot.getStartDate().getTime()));
		insertStatement.setTimestamp(3, new java.sql.Timestamp(timeSlot.getEndDate().getTime()));
		insertStatement.setString(4, description);
		insertStatement.setString(5, location);
		insertStatement.setBoolean(6, isGroupAppointment);

		int rowCount = insertStatement.executeUpdate();
		assert rowCount == 1 : "the row count was not 1 after inserting into the Appointments table";

		// put this object in the map of loaded objects
		AppointmentFinder.getInstance().addToMap(id + "", this);
	}

	/**
	 * Updates this appointment in the database.
	 * <br /><br />
	 * Be aware that the id cannot be changed. This means that if you do a setId(some-new-id) and then a dbUpdate()
	 * you will probably get an error message saying that an appointment with some-new-id cannot be found in the
	 * database (or, even worse, if an(other) appointment with that some-new-id does exist in the database, then you
	 * will end up updating the data from that other appointment, which is probably not what you intend).
	 *
	 * @param conn a valid connection to the database.
	 * @throws SQLException if an SQLException occurs while communicating to the database, it is forwarded to the
	 *	caller.
	 * @throws InvalidAppointmentIdException if the update fails (probably) due to the fact that the appointement id
	 *	does not identify a valid appointment from the database.
	 */
	public void dbUpdate(Connection conn) throws SQLException, InvalidAppointmentIdException
	{
		PreparedStatement updateStatement = conn.prepareStatement(updateAppointmentStatement());
		updateStatement.setTimestamp(1, new java.sql.Timestamp(timeSlot.getStartDate().getTime()));
		updateStatement.setTimestamp(2, new java.sql.Timestamp(timeSlot.getEndDate().getTime()));
		updateStatement.setString(3, description);
		updateStatement.setString(4, location);
		updateStatement.setBoolean(5, isGroupAppointment);
		updateStatement.setInt(6, id);
		if (updateStatement.executeUpdate() == 0)
		{
			throw new InvalidAppointmentIdException();
		}
		else
		{
			// update the object in the map of loaded objects as well
			AppointmentFinder.getInstance().addToMap(id + "", this);
		}
	}

	/**
	 * Deletes this appointment from the database.
	 *
	 * @param conn a valid connection to the database.
	 * @throws SQLException if an SQLException occurs while communicating to the database, it is forwarded to the
	 *	caller.
	 * @throws InvalidAppointmentIdException if the deletion fails (probably) due to the fact that the appointement
	 *	id does not identify a valid appointment from the database.
	 */
	public void dbDelete(Connection conn) throws SQLException, InvalidAppointmentIdException
	{
		PreparedStatement deleteStatement = conn.prepareStatement(deleteAppointmentStatement());
		deleteStatement.setInt(1, id);
		if (deleteStatement.executeUpdate() == 0)
		{
			throw new InvalidAppointmentIdException();
		}
		else
		{
			// remove this object from the map of loaded objects
			AppointmentFinder.getInstance().removeFromMap(id + "");
		}
	}

	private static String nextValStatement()
	{
		return "SELECT nextval('Appointments_id_seq')";
	}

	private static String insertAppointmentStatement()
	{
		return "INSERT INTO Appointments" +
			" VALUES (?, ?, ?, ?, ?, ?)";
	}

	private static String updateAppointmentStatement()
	{
		return "UPDATE Appointments" +
			" SET startTime = ?, endTime = ?, description = ?, location = ?, isgroupapp = ?" +
			" WHERE id = ?";
	}

	private static String deleteAppointmentStatement()
	{
		return "DELETE FROM Appointments" +
			" WHERE id = ?";
	}

	private int id;
	private Period timeSlot;
	private String location;
	private String description;
	private boolean isGroupAppointment;
}
