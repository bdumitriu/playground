import java.io.*;

public class CitireIntreg
{
	public static void main(String args[])
	{
		byte[] temp = new byte[10];
		int nrOfReadBytes;
		
		System.out.print("Dati o valoare intreaga: ");
		try
		{
			// aici citesc un array de bytes (octeti)
			// care reprezinta date neformatate (singura
			// chestie care se poate citi direct in Java).
			// functia read imi intoarce numarul de octeti
			// cititi efectiv
			nrOfReadBytes = System.in.read(temp);
			
			// ca sa construiesc un intreg din datele respective
			// trebuie sa fac toata tarasenia care urmeaza...
			
			// construiesc un String din datele neformatate.
			// Am scazut 2 din numarul de octeti pentru a scapa
			// de caracterul sfarsit de linie (echivalentul lui
			// \n din C. In Java, acest caracter sfarsit de linie
			// ocupa doi octeti
			String str = new String(temp, 0, nrOfReadBytes-2);
			
			// si acum construiesc un intreg din String-ul creat
			// inainte
			int n = Integer.parseInt(str);
			
			// si il afisez
			System.out.println("Intregul introdus a fost " + n);
		}
		catch (IOException e)
		{
			System.out.println("A avut loc o exceptie la citire.");
		}
		catch (NumberFormatException e)
		{
			System.out.println("Nu ati introdus un intreg valid.");
		}
	}
}