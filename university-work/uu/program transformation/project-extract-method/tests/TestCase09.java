/**
 * Here we return in the extracted fragment => disallowed.
 */
public class TestCase09
{
	public void testMethod()
	{
		int x = 6;

		@ emTestMethod
		if (x == 5)
		{
			return;
		}
		else
		{
			x--;
		}
		@

		return;
	}
}
