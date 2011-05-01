public class Benchmark
{
	public static void main(String args[])
	{
		AnInterface o1 = new AClass();
		AnotherClass o2 = new AnotherClass();
		
		System.out.println("Please wait...");
		
		long t1 = System.currentTimeMillis();
		for (int i = 0; i < Integer.MAX_VALUE; i++)
		{
			o1.aMethod();
		}
		long t2 = System.currentTimeMillis();
		
		System.out.println("Calling interfaced method lots of times took " + (t2-t1) + "ms.");
		System.out.println("Please wait again...");
		
		t1 = System.currentTimeMillis();
		for (int i = 0; i < Integer.MAX_VALUE; i++)
		{
			o2.aMethod();
		}
		t2 = System.currentTimeMillis();
		
		System.out.println("Calling non-interfaced method lots of times took " + (t2-t1) + "ms.");
	}
}

interface AnInterface
{
	public void aMethod();
}

class AClass implements AnInterface
{
	public void aMethod()
	{}
}

class AnotherClass
{
	public void aMethod()
	{}
}