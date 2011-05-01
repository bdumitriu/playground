import java.io.*;

public class AlgoritmImpartireBinara
{
	public static void main(String args[])
	{
		int x = 144;
		int y = 13;
		int q = 0;
		int r = 0;
		
		r = x>>7;
		pr(r);
		r = r-y;
		for (int i = 0; i < 7; i++)
		{
			pr(r);
			if (r < 0)
			{
				q = 2*q;
				pr(q);
				r = 2*r+y;
				pr(r);
			}
			else
			{
				q = 2*q+2;
				pr(q);
				r = 2*r-y;
				pr(r);
			}
		}
		
		
		if (r < 0)
		{
			r = r+y;
			pr(r);
		}
		else
		{
			q = q+1;
			pr(q);
		}
		
		System.out.println("q =\t" + q);
		System.out.println("r =\t" + r);
	}
	
	private static void pr(int x)
	{
		System.out.println(x);
		try
		{
			System.in.read();
			System.in.skip(System.in.available());
		}
		catch (Exception e)
		{}
	}
}