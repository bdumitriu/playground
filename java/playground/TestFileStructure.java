import java.io.*;

public class TestFileStructure {
	public static void main(String args[]) {
		File f = new File("C:" + File.separator);
		File files[] = f.listFiles();
		f = new File("C:\\");
		files = f.listFiles();
		for (int i = 0; i < files.length; i++) {
			System.out.print(files[i].getPath() + "\t\t");
			if (files[i].isDirectory() == true)
				System.out.println("directory");
			else
				System.out.println("file");
		}
	}
}