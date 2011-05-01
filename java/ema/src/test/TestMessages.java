package test;

import shared.EMAErrorMessages;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 7, 2003
 */
public class TestMessages
{
	public static void main(String[] args)
	{
		EMAErrorMessages em = EMAErrorMessages.getInstance();

		System.out.println(em.getMessage("error.system"));
	}
}
