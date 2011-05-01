/**
 * Here we have another combination of variables.
 * Although x is assigned to in the fragment, it is not used afterwards,
 * so it doesn't have to be returned.
 */
public class TestCase07
{
	public void testMethod()
	{
		int x;
		int z;

		@ emTestMethod
		x = methodCall();
		
		boolean y = true;

		System.out.print(x);
		System.out.print(y);
		System.out.print(z);
		@
	}
}
