import java.io.*;
import java.util.Vector;

public class PipedSerialization
{
	public static void main(String args[])
	{
		try
		{
			PipedOutputStream out = new PipedOutputStream();
			PipedInputStream in = new PipedInputStream(out);
			Outputter o = new Outputter(out);
			Inputter i = new Inputter(in);
			
			o.start();
			i.start();
		}
		catch (IOException e)
		{
			System.out.println(e.toString());
		}
	}
}

class Outputter extends Thread
{
	PipedOutputStream out;

	public Outputter(PipedOutputStream out)
	{
		this.out = out;
	}
	
	public void run()
	{
		SomeObject o = new SomeObject();
		try
		{
			ObjectOutputStream oos = new ObjectOutputStream(out);
			o.addElement(new String("xyz"));
			o.addElement(new Integer(54));
			o.addElement(new Character('a'));
			oos.writeObject(o);
			System.out.println("This is what's been serialized\n"
				+ o.toString());
		}
		catch (IOException e)
		{
			System.out.println(e.toString());
		}
	}
}

class Inputter extends Thread
{
	PipedInputStream in = new PipedInputStream();
	
	public Inputter(PipedInputStream in)
	{
		this.in = in;
	}
	
	public void run()
	{
		SomeObject o;
		try
		{
			ObjectInputStream ois = new ObjectInputStream(in);
			o = (SomeObject) ois.readObject();
			System.out.println("This is what's been" +
				" deserialized\n" + o.toString());
		}
		catch (IOException e)
		{
			System.out.println(e.toString());
		}
		catch (ClassNotFoundException e)
		{
			System.out.println(e.toString());
		}
	}
}

class SomeObject implements Serializable
{
	private int x, y;
	private String z;
	private SomeOtherObject o;
	private Vector v;
	
	public SomeObject()
	{
		this(10, 10, "blank message", new SomeOtherObject());
	}
	
	public SomeObject(int x, int y, String z)
	{
		this(x, y, z, new SomeOtherObject());
	}
	
	public SomeObject(int x, int y, String z, SomeOtherObject o)
	{
		this.x = x;
		this.y = y;
		this.z = z;
		this.o = o;
		v = new Vector();
	}
	
	public void addElement(Object o)
	{
		v.addElement(o);
	}
	
	public String toString()
	{
		return "x is: " + x + "; y is: " + y + ";\nz is: " + z +
			";\no's mes is: " + o.getMes() + "\nv is: " +
			v.toString() + "\n";
	}
}

class SomeOtherObject implements Serializable
{
	private String mes;
	
	public SomeOtherObject()
	{
		this("nothing here...");
	}
	
	public SomeOtherObject(String mes)
	{
		this.mes = mes;
	}
	
	public String getMes()
	{
		return mes;
	}
	
	public void setMes(String mes)
	{
		this.mes = mes;
	}
}
