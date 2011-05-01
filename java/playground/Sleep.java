public class Sleep
{
	public static void main(String a[])
	{
		System.out.println("Sleeping started...");
		try
		{
			Thread.sleep(5000);
		}
		catch (InterruptedException e)
		{
			e.toString();
		}
		System.out.println("Slept for 5 seconds...");
	}
}
