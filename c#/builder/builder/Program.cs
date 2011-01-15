using System;

namespace builder
{
	class Program
	{
		public static void Main(string[] args)
		{
			new Director(new HtmlFormBuilder("out.html")).Build("spec.xml");
			Console.ReadKey(true);
		}
	}
}
