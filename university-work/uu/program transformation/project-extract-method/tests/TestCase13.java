/**
 * Here we have a declaration inside the fragment which needs to be moved
 * outside it. However, it is on a list of declarations, which must be broken
 * into separate declarations.
 */

import java.util.ArrayList;

public class TestCase13
{
	public void testMethod(int a, boolean b, ArrayList al)
	{
		int x;
		String m = ".";

		x = methodCall();
		
		boolean y = true;

		@ emTestMethod
		x = 1;
		
		String aa, bb = "r", z = m, cc = "t";
		System.out.print(x);
		System.out.print(y);
		System.out.print(z);
		System.out.print(a);
		System.out.print(al);
		@

		z.toString();
	}

	private String z;
}
