package test;

import java.util.*;
import java.text.*;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Dec 6, 2003
 * Time: 3:12:12 PM
 * To change this template use Options | File Templates.
 */
public class TestI18N
{
	public static void main(String[] args)
	{
		Locale list[] = DateFormat.getAvailableLocales();
		for (int i = 0; i < list.length; i++)
		{
			System.out.println(list[i].getDisplayName());
		}

	}
}
