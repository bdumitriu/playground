import java.awt.*;
import java.awt.event.*;
import java.io.*;

class RenameFiles
{

	private class FrameManager extends WindowAdapter
	{

		public void windowClosing(WindowEvent e)
		{
			System.exit(0);
		}
	}

	public RenameFiles()
	{
		Frame f = new Frame("Rename files utility...");
		Button renameButton = new Button("Rename all files");
		FilePanel filePanel = null;

		try
		{
			filePanel = new FilePanel("C:\\");
		}
		catch (NotADirectoryException e)
		{
			System.out.println(e.getMessage());
		}

		f.setLayout(new GridLayout(1,3));
		f.setBounds(300, 300, 400, 300);
		f.setResizable(true);
		f.addWindowListener(new FrameManager());
		renameButton.addActionListener(new ButtonActionListener(filePanel));
		f.add(filePanel);
		f.add(renameButton);

		f.setVisible(true);
	}

	private class ButtonActionListener implements ActionListener
	{

		private FilePanel filePanel;

		public ButtonActionListener(FilePanel filePanel)
		{
			this.filePanel = filePanel;
		}

		public void actionPerformed(ActionEvent e)
		{
			File curDir = new File(filePanel.getCurrentDirectory());
//			File files[] = curDir.listFiles();

//			for (int i = 0; i < files.length; i++)
//				changeAll(files[i]);

			changeAll(curDir);

/*			String nameStart = curDir.toString() + "\\";
			nameStart += curDir.getName().toLowerCase().replace(' ', '_').replace('-', '_');
			String name;
			int j = 0;

			for (int i = 0; i < files.length; i++)
			{
				if (files[i].isFile() == true)
				{
					j++;
					name = nameStart;
					if (((int) (j/10)) == 0)
						name += "00";
					else if (((int) (j/100)) == 0)
						name += "0";
					name += String.valueOf(j);
					String fn = files[i].getName();
					if (fn.lastIndexOf('.') != -1)
						name += fn.substring(fn.lastIndexOf('.'));
					File newFile = new File(name);
					files[i].renameTo(newFile);
				}
			}*/

		}

		private void changeAll(File curDir) {
			File files[] = curDir.listFiles();
			String nameStart = curDir.toString() + "\\";
			nameStart += curDir.getName().toLowerCase().replace(' ', '_').replace('-', '_');
			String name;
			int j = 0;

			for (int i = 0; i < files.length; i++) {
				if (files[i].isFile() == true)
				{
					j++;
					name = nameStart;
					if (((int) (j/10)) == 0)
						name += "00";
					else if (((int) (j/100)) == 0)
						name += "0";
					name += String.valueOf(j);
					String fn = files[i].getName();
					if (fn.lastIndexOf('.') != -1)
						name += fn.substring(fn.lastIndexOf('.')).toLowerCase();
					File newFile = new File(name);
					files[i].renameTo(newFile);
				}
			}
		}
	}
}

public class RenameFilesUtility {

	public static void main(String args[]) {
		RenameFiles window = new RenameFiles();
	}
}