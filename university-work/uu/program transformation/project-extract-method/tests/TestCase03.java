/**
 * Here we have some variables in the extracted code, but they are local to the
 * extracted code.
 */
public class TestCase03
{
	public void testMethod()
	{
		System.out.print("header");

		@ emTestMethod
		int i;
		
		for (i = 0; i < 5; i++)
		{
			System.out.print("some text");
		}
		@

		System.out.print("footer");
	}
}
