/**
 * This is the first test case in which we need to add some parameters to the
 * extracted method. This is a simple case where we don't change the variables
 * in the extracted method.
 */
public class TestCase04
{
	public void testMethod()
	{
		System.out.print("header");
		int x;

		@ emTestMethod
		int i;
		for (i = 0; i < 5; i++)
		{			
			i += x;
		}
		@

		System.out.print(x);
		System.out.print("footer");
	}
}
