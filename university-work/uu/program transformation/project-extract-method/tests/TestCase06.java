/**
 * Here we have several types of variables:
 * z - declared outside, but not used => it's not added to the parameter list.
 * x - declared outside, assigned to inside, used afterwards => assign the
 *     result of the method to it.
 * y - declared inside and used afterwards => declaration moved up before the
 *     method call. y is not actually used in the fragment, so it won't be
 *     added to the parameter list.
 * t - declared inside the method and not used afterwards => not moved up
 */
public class TestCase06
{
	public int testMethod()
	{
		int x, z;
		
		@ emTestMethod
		int y;
		int t;
		x += 2;
		x = x + 1;
		if (true)
			x += 5;
		@

		y = y + x;

		System.out.print(x);
		return x;
	}
}
