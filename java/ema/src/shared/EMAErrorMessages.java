/**
 * This class provides a wrapper over the error messages properties file.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 7, 2003
 */
package shared;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;

public class EMAErrorMessages
{
	public synchronized static EMAErrorMessages getInstance()
	{
		if (instance == null)
		{
			instance = new EMAErrorMessages();
		}

		return instance;
	}

	/**
	 * Returns the error message associated with the message key from the properties file.
	 *
	 * @param messageKey the message key identifying the error message
	 */
	public String getMessage(String messageKey)
	{
		return messageBundle.getString(messageKey);
	}

	private EMAErrorMessages()
	{
		EMALogger logger = EMALogger.getInstance();
		try
		{
			logger.logConfigMessage("Trying to read error messages from file.");
			messageBundle = ResourceBundle.getBundle(
			        EMAProperties.getInstance().getProperty("messages.error"),
			        EMAProperties.getInstance().getLocale());
			logger.logConfigMessage("Error messages read successfully.");
		}
		catch (MissingResourceException e)
		{
			Locale locale = EMAProperties.getInstance().getLocale();
			Locale defaultLocale = Locale.getDefault();

			logger.logErrorMessage("Could not find any file containing values of error " +
				"messages. The search path was CLASSPATH/messages and the files looked for were:" +
				"\nerror-messages_" + locale.getLanguage() + "_" + locale.getCountry() + "_" +
				locale.getVariant() + ".properties" + "\nerror-messages_" + locale.getLanguage() + "_" +
				locale.getCountry() + ".properties" + "\nerror-messages_" + locale.getLanguage() +
				".properties" + "\nerror-messages_" + defaultLocale.getLanguage() + "_" +
				defaultLocale.getCountry() + ".properties" + "\nerror-messages_" +
				defaultLocale.getLanguage() + ".properties" +
				"\nerror-messages.properties\nNone of these files could be found.");

			System.exit(1);
		}
	}

	/**
	 * The bundle of error messages.
	 */
	ResourceBundle messageBundle;

	/**
	 * An instance of this class.
	 */
	private static EMAErrorMessages instance;
}

