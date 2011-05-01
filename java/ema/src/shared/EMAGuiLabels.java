package shared;

import java.util.ResourceBundle;
import java.util.MissingResourceException;
import java.util.Locale;
import java.awt.event.KeyEvent;

/**
 * This class provides a wrapper over the graphical user interface labels properties file.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 13, 2003
 */
public class EMAGuiLabels
{
	public synchronized static EMAGuiLabels getInstance()
	{
		if (instance == null)
		{
			instance = new EMAGuiLabels();
		}

		return instance;
	}

	/**
	 * Returns the label associated with the message key from the properties file.
	 *
	 * @param messageKey the message key identifying the label
	 */
	public String getMessage(String messageKey)
	{
		return messageBundle.getString(messageKey);
	}

	/**
	 * Returns the label associated with the message key from the properties file modified by removing the & from
	 * the label, if there is one.
	 *
	 * @param messageKey the message key identifying the label
	 */
	public String getModifiedMessage(String messageKey)
	{
		String text = messageBundle.getString(messageKey);

		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < text.length(); i++)
		{
			if (text.charAt(i) != '&')
			{
				sb.append(text.charAt(i));
			}
		}

		return sb.toString();
	}

	/**
	 * Returns the integer representing the mnemonic to associate with the label identified by message key. This is
	 * done by looking at the character coming after the first & character and converting it to a KeyEvent constant.
	 * If no & appears in the label or if the first & precedes a character which is neither a lower- or uppercase
	 * letter nor a digit then a KeyEvent.VK_UNDEFINED is returned.
	 *
	 * @param messageKey the message key identifying the label from which the mnemonic is obtained
	 * @return the mnemonic associated with the label or KeyEvent.VK_UNDEFINED if no valid mnemonic is obtainable
	 */
	public int getMnemonicFrom(String messageKey)
	{
		String text = messageBundle.getString(messageKey);

		int index = text.indexOf('&');
		if ((index == -1) || (index + 1 == text.length()))
		{
			return KeyEvent.VK_UNDEFINED;
		}

		char ch = text.charAt(index + 1);
		if (!Character.isLetterOrDigit(ch))
		{
			return KeyEvent.VK_UNDEFINED;
		}

		switch (ch)
		{
			case 'a':
			case 'A':
				return KeyEvent.VK_A;
			case 'b':
			case 'B':
				return KeyEvent.VK_B;
			case 'c':
			case 'C':
				return KeyEvent.VK_C;
			case 'd':
			case 'D':
				return KeyEvent.VK_D;
			case 'e':
			case 'E':
				return KeyEvent.VK_E;
			case 'f':
			case 'F':
				return KeyEvent.VK_F;
			case 'g':
			case 'G':
				return KeyEvent.VK_G;
			case 'h':
			case 'H':
				return KeyEvent.VK_H;
			case 'i':
			case 'I':
				return KeyEvent.VK_I;
			case 'j':
			case 'J':
				return KeyEvent.VK_J;
			case 'k':
			case 'K':
				return KeyEvent.VK_K;
			case 'l':
			case 'L':
				return KeyEvent.VK_L;
			case 'm':
			case 'M':
				return KeyEvent.VK_M;
			case 'n':
			case 'N':
				return KeyEvent.VK_N;
			case 'o':
			case 'O':
				return KeyEvent.VK_O;
			case 'p':
			case 'P':
				return KeyEvent.VK_P;
			case 'q':
			case 'Q':
				return KeyEvent.VK_Q;
			case 'r':
			case 'R':
				return KeyEvent.VK_R;
			case 's':
			case 'S':
				return KeyEvent.VK_S;
			case 't':
			case 'T':
				return KeyEvent.VK_T;
			case 'u':
			case 'U':
				return KeyEvent.VK_U;
			case 'v':
			case 'V':
				return KeyEvent.VK_V;
			case 'w':
			case 'W':
				return KeyEvent.VK_W;
			case 'x':
			case 'X':
				return KeyEvent.VK_X;
			case 'y':
			case 'Y':
				return KeyEvent.VK_Y;
			case 'z':
			case 'Z':
				return KeyEvent.VK_Z;
			case '0':
				return KeyEvent.VK_0;
			case '1':
				return KeyEvent.VK_1;
			case '2':
				return KeyEvent.VK_2;
			case '3':
				return KeyEvent.VK_3;
			case '4':
				return KeyEvent.VK_4;
			case '5':
				return KeyEvent.VK_5;
			case '6':
				return KeyEvent.VK_6;
			case '7':
				return KeyEvent.VK_7;
			case '8':
				return KeyEvent.VK_8;
			case '9':
				return KeyEvent.VK_9;
			default:
				return KeyEvent.VK_UNDEFINED;
		}
	}

	private EMAGuiLabels()
	{
		EMALogger logger = EMALogger.getInstance();
		try
		{
			logger.logConfigMessage("Trying to read GUI labels from file.");
			messageBundle = ResourceBundle.getBundle(
			        EMAProperties.getInstance().getProperty("messages.gui"),
			        EMAProperties.getInstance().getLocale());
			logger.logConfigMessage("GUI labels read successfully.");
		}
		catch (MissingResourceException e)
		{
			Locale locale = EMAProperties.getInstance().getLocale();
			Locale defaultLocale = Locale.getDefault();

			logger.logErrorMessage("Could not find any file containing values of gui labels. The search " +
				"path was CLASSPATH/messages and the files looked for were:" +
				"\ngui-labels_" + locale.getLanguage() + "_" + locale.getCountry() + "_" +
				locale.getVariant() + ".properties" + "\ngui-labels_" + locale.getLanguage() + "_" +
				locale.getCountry() + ".properties" + "\ngui-labels_" + locale.getLanguage() +
				".properties" + "\ngui-labels_" + defaultLocale.getLanguage() + "_" +
				defaultLocale.getCountry() + ".properties" + "\ngui-labels_" +
				defaultLocale.getLanguage() + ".properties" +
				"\ngui-labels.properties\nNone of these files could be found.");

			System.exit(1);
		}
	}

	/**
	 * The bundle of gui labels.
	 */
	ResourceBundle messageBundle;

	/**
	 * An instance of this class.
	 */
	private static EMAGuiLabels instance;
}

