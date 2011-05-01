/**
 * This class provides the system with an simple means of logging all types of
 * messages to a file.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 6, 2003
 */
package shared;

import java.util.logging.FileHandler;
import java.util.logging.LogRecord;
import java.util.logging.Level;
import java.util.logging.SimpleFormatter;
import java.io.IOException;
import java.sql.SQLException;
import java.rmi.RemoteException;

public class EMALogger
{
	public synchronized static EMALogger getInstance()
	{
		if (logger == null)
		{
			logger = new EMALogger();
		}

		return logger;
	}

	/**
	 * Logs a configuration message.
	 *
	 * @param message the message text
	 */
	public void logConfigMessage(String message)
	{
		if (fileHandler == null)
		{
			return;
		}

		LogRecord record = new LogRecord(Level.CONFIG, message);
		record.setLoggerName("");
		fileHandler.publish(record);
	}

	/**
	 * Logs an informational message.
	 *
	 * @param message the message text
	 */
	public void logInfoMessage(String message)
	{
		if (fileHandler == null)
		{
			return;
		}

		LogRecord record = new LogRecord(Level.INFO, message);
		record.setLoggerName("");
		fileHandler.publish(record);
	}

	/**
	 * Logs an error message.
	 *
	 * @param message the message text
	 */
	public void logErrorMessage(String message)
	{
		if (fileHandler == null)
		{
			return;
		}

		LogRecord record = new LogRecord(Level.SEVERE, message);
		record.setLoggerName("");
		fileHandler.publish(record);
	}

	/**
	 * Logs any other type of message than the ones defined above.
	 *
	 * @param message the message text
	 */
	public void logOtherMessage(String message)
	{
		if (fileHandler == null)
		{
			return;
		}

		LogRecord record = new LogRecord(Level.FINEST, message);
		record.setLoggerName("");
		fileHandler.publish(record);
	}

	/**
	 * Logs a default error message for SQL exceptions. This method is provided
	 * because the system makes heavy use of methods throwing this exception and
	 * a uniform error message for all these exceptions is preferred.
	 *
	 * @param e the SQLException
	 */
	public void logDefaultSQLExceptionMessage(SQLException e)
	{
		logErrorMessage("An error occured while using the database. The exact error message given by the " +
			"system was: \"" + e.getMessage() + "\".");
	}

	/**
	 * Logs a default error message for remote exceptions. This method is provided
	 * because the system makes heavy use of methods throwing this exception and
	 * a uniform error message for all these exceptions is preferred.
	 *
	 * @param e the RemoteException
	 */
	public void logDefaultRemoteExceptionMessage(RemoteException e)
	{
		logErrorMessage("An error occured while using the RMI mechanism. The exact error message given by " +
			"the system was: \"" + e.getMessage() + "\".");
	}

	private EMALogger()
	{
		try
		{
			// create the FileHandler which will write to the log file
			EMAProperties props = EMAProperties.getInstance();
			fileHandler = new FileHandler(props.getProperty("log.file"), true);

			// set the log level
			String logLevel = props.getProperty("log.level");
			if (logLevel.equalsIgnoreCase("none"))
			{
				fileHandler.setLevel(Level.OFF);
			}
			else if (logLevel.equalsIgnoreCase("config"))
			{
				fileHandler.setLevel(Level.CONFIG);
			}
			else if (logLevel.equalsIgnoreCase("info"))
			{
				fileHandler.setLevel(Level.INFO);
			}
			else if (logLevel.equalsIgnoreCase("error"))
			{
				fileHandler.setLevel(Level.SEVERE);
			}
			else if (logLevel.equalsIgnoreCase("all"))
			{
				fileHandler.setLevel(Level.ALL);
			}

			fileHandler.setFormatter(new SimpleFormatter());
		}
		catch (IOException e)
		{
			fileHandler = null;
		}
		catch (SecurityException e)
		{
			fileHandler = null;
		}
	}

	/**
	 * The FileHandler used to log messages.
	 */
	FileHandler fileHandler;

	/**
	 * An instance of this class.
	 */
	private static EMALogger logger;
}

