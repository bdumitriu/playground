/**
 * Here the fragment declares a variable (z) which is used outside the fragment.
 * There is also a variable (x) assigned to inside the fragment, but not used
 * afterwards, so in the end we only have one variable to return, after all (z),
 * so extraction is possible.
 */

import java.util.ArrayList;

public class TestCase08
{
	public void testMethod(int a, boolean b, ArrayList al)
	{
		int x;

		x = methodCall();
		
		boolean y = true;

		@ emTestMethod
		x = 1;
		
		String z = "some text";
		System.out.print(x);
		System.out.print(y);
		System.out.print(z);
		System.out.print(a);
		System.out.print(al);
		@

		z = "";
	}

	private String z;
}
