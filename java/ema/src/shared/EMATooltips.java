package shared;

import java.util.ResourceBundle;
import java.util.MissingResourceException;
import java.util.Locale;

/**
 * This class provides a wrapper over the error messages properties file.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 13, 2003
 */
public class EMATooltips
{
	public synchronized static EMATooltips getInstance()
	{
		if (instance == null)
		{
			instance = new EMATooltips();
		}

		return instance;
	}

	/**
	 * Returns the tooltip associated with the message key from the properties file.
	 *
	 * @param messageKey the message key identifying the tooltip
	 */
	public String getMessage(String messageKey)
	{
		return messageBundle.getString(messageKey);
	}

	private EMATooltips()
	{
		EMALogger logger = EMALogger.getInstance();
		try
		{
			logger.logConfigMessage("Trying to read tooltips from file.");
			messageBundle = ResourceBundle.getBundle(
			        EMAProperties.getInstance().getProperty("messages.tooltip"),
			        EMAProperties.getInstance().getLocale());
			logger.logConfigMessage("Tooltips read successfully.");
		}
		catch (MissingResourceException e)
		{
			Locale locale = EMAProperties.getInstance().getLocale();
			Locale defaultLocale = Locale.getDefault();

			logger.logErrorMessage("Could not find any file containing values of tooltips. The search " +
				"path was CLASSPATH/messages and the files looked for were:" +
				"\ntooltips_" + locale.getLanguage() + "_" + locale.getCountry() + "_" +
				locale.getVariant() + ".properties" + "\ntooltips_" + locale.getLanguage() + "_" +
				locale.getCountry() + ".properties" + "\ntooltips_" + locale.getLanguage() +
				".properties" + "\ntooltips_" + defaultLocale.getLanguage() + "_" +
				defaultLocale.getCountry() + ".properties" + "\ntooltips_" +
				defaultLocale.getLanguage() + ".properties" +
				"\ntooltips.properties\nNone of these files could be found.");

			System.exit(1);
		}
	}

	/**
	 * The bundle of tooltips.
	 */
	ResourceBundle messageBundle;

	/**
	 * An instance of this class.
	 */
	private static EMATooltips instance;

}
