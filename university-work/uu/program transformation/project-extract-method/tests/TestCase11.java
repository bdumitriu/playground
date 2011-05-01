/**
 * In this test we have a more complex class and method structure.
*/
class YetAnotherClass
{
	public void testMethod()
	{

		for (int i = 0; i < 10; i++)
		{
			System.out.println(i);
			return;
		}
	}
}

public class TestCase11
{
	public int whatever()
	{
		int x = 5;
		return x;
	}

	public void testMethod()
	{

		for (int i = 0; i < 10; i++)
		{
			@ emTestMethod
			System.out.println(i);
			@
			return;
		}
	}
	
	public int whatever(int y)
	{
		return y;
	}
}


class SomeOtherClass
{
	public void testMethod()
	{

		for (int i = 0; i < 10; i++)
		{
			System.out.println(i);
			return;
		}
	}
}
