/**
 * Here we are assigning to more than one variable in the fragment =>
 * disallowed.
 */
public class TestCase10
{
	public void testMethod()
	{
		int x, y;
		@ emTestMethod
		x = y = 1;
		@
		x = y = 1;
	}
}
