package test;

import shared.EMADefaultProperties;
import shared.EMAProperties;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 6, 2003
 */
public class TestProperties
{
	public static void main(String[] args)
	{
		EMAProperties props = EMAProperties.getInstance();
		System.out.println(props.toString());
	}
}
