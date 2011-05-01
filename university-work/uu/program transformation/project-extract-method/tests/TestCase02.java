/**
 * More or less like the previous one, except that there is some more code
 * around the method to be extracted
 */
public class TestCase02
{
	public void testMethod()
	{
		System.out.print("header");

		@ emTestMethod
		System.out.print("some text");
		System.out.print("some more text");
		@

		System.out.print("footer");
	}
}
