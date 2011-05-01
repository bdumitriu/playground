package test;

import shared.EMALogger;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 6, 2003
 */
public class TestLogger
{
	public static void main(String[] args)
	{
		EMALogger logger = EMALogger.getInstance();

		logger.logConfigMessage("Config message.");
		logger.logInfoMessage("Info message.");
		logger.logErrorMessage("Error message.");
		logger.logOtherMessage("Other message.");
	}
}
