package shared;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 6, 2003
 */
public class EMADefaultProperties
{
	/**
	 * All the property names which are suppoed to apper in the
	 * EMA.properties file.
	 */
	public static String[] allKeys =
	        {
		        "locale.language",
		        "locale.country",
		        "locale.system",
		        "server.ip",
		        "dbserver.ip",
		        "dbserver.port",
		        "dbserver.user",
		        "dbserver.password",
		        "dbserver.database",
		        "rmiserver.port",
		        "log.file",
		        "log.level",
		        "messages.error",
		        "messages.gui",
		        "messages.tooltips"
	        };

	/**
	 * All the property values which can apper in the EMA.properties file.
	 * The values should correspond to the keys in the {@link #allKeys
	 * allKeys} array by position (i.e. the first array in {@link
	 * #allValues allValues} should contain the allowed values for the
	 * first key in {@link #allKeys allKeys}.
	 *
	 * The first entry in each array represents the default value to be
	 * used. All the other entries represent the allowed values. The first
	 * entry should be duplicated.
	 *
	 * If for some property any possible value is allowed, the corresponding
	 * array should only contain one value (i.e. the default value to be
	 * used when the property itself does not appear in the configuration
	 * file or when the configuration file is missing).
	 */
	public static String[][] allValues =
	        {
		        {"ro", "ro", "en"},
		        {"RO", "RO", "US"},
		        {"unix", "unix", "windows"},
		        {"127.0.0.1"},
		        {"127.0.0.1"},
		        {"5432"},
		        {"ema"},
		        {"ema"},
		        {"ema"},
		        {"2500"},
		        {"log/EMA.log"},
		        {"error", "none", "config", "info", "error", "all"},
		        {"messages/error-messages"},
		        {"messages/gui-labels"},
		        {"messages/tooltips"}
	        };
}
