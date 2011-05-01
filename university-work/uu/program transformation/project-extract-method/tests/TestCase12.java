/**
 * Here we are using an anonymous class inside the fragment. 
 */
public class TestCase12
{
	public void testMethod()
	{
		for (int i = 0; i < 10; i++)
		{
			@ emTestMethod
			System.out.println(i);
			anotherMethodCall(new Object());
			methodCall(new Object()
				{
					public void someMethod()
					{
						// some code
					}
				});
			@
			return;
		}
	}
}
