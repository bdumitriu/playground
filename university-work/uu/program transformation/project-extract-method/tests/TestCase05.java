/**
 * Here we have an assignment to a variable, which is converted to an
 * extracted method which returns the changed variable, and the parent method
 * must assign the variable to the return of the new method.
 */
import java.util.ArrayList;

public class TestCase05
{
	public void testMethod()
	{
		System.out.print("header");
		ArrayList x = new ArrayList();

		@ emTestMethod
		x = new ArrayList();
		@

		System.out.print(x);
		System.out.print("footer");
	}
}
