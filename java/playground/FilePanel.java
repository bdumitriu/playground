import java.awt.Panel;
import java.awt.List;
import java.awt.GridLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Vector;

/*
	this class offers the user a Component that manages the directory
	structure on the disk
*/
public class FilePanel extends Panel {

	private File curPath;
	private List curFileList;
	private Vector selections;

	// the startingDirectory is the directory whose contents will
	// be shown on the screen when the object is first set visible
	public FilePanel(String startingDirectory) throws NotADirectoryException {
		super(new GridLayout());
		if ((new File(startingDirectory)).isDirectory() == true) {
			selections = new Vector();
			go(startingDirectory, 'f');
		}
		else
			throw new NotADirectoryException();
	}

	public String getCurrentDirectory() {
		return curPath.toString();
	}

	// gets the list of files in directory path.
	// If where is 'b' (from backwards) then it looks in the
	// selections vector to see if there is a value for the
	// index. If vector is null then the index defaults to 0.
	private List getListOfFiles(String path, char where) {
		List list = new List();
		File f = new File(path);
		File files[] = f.listFiles();
		int sel = 0;

		if ((where == 'b') && (selections.size() != 0)) {
			Integer selection = (Integer) selections.lastElement();
			sel = selection.intValue();
			selections.removeElementAt(selections.size()-1);
		}

		Arrays.sort(files, new FileTypeComparator());
		if (!(f.toString().equals("C:\\")))
			list.add("..");
		for (int i = 0; i < files.length; i++)
			if (files[i].isDirectory() == true)
				list.add("[" + files[i].getName() + "]");
			else
				list.add(files[i].getName());
		list.addActionListener(new ListActionListener(list));
		list.addKeyListener(new ListKeyListener(list));
		list.select(sel);

		return list;
	}

	// updates the panel with the new list when the directory
	// is changed.
	private void go(String path, char where) {
		curPath = new File(path);
		curFileList = getListOfFiles(path, where);

		this.removeAll();
		this.add(curFileList);
		this.validate();
		curFileList.requestFocus();
	}











	/*
		this implementation of the Comparator interface compares
		objects of the type java.io.File. Its methods behave in
		such a way that directories are returned to be smaller
		than files. It also makes an alphabetical comparison
		between files on one hand and directories on the other
	*/
	private class FileTypeComparator implements Comparator {

		// returns -1 if o1 is a directory and o2 is a file
		// returns +1 if o1 is a file and o2 is a directory
		// if o1 and o2 are both either files or directories
		// than it returns -1, 0 or 1 as lowerCase(o1) is
		// smaller that, equal to or greater than lowerCase(o2)
		public int compare(Object o1, Object o2) {
			File f1 = (File) o1;
			File f2 = (File) o2;
			int n1, n2;
			
			if (f1.isDirectory() == true)
				n1 = 1;
			else
				n1 = 2;

			if (f2.isDirectory() == true)
				n2 = 1;
			else
				n2 = 2;

			if (n1 == n2)
				return (f1.getName().toLowerCase().compareTo(f2.getName().toLowerCase()));
			else if (n1 < n2)
				return -1;
			else
				return 1;
		}

		// returns true if this and obj are of the same type
		// (either files or directories) and have the same
		// lower-cased name and false otherwise
		public boolean equals(Object obj) {
			if (compare(this, obj) == 0)
				return true;
			else
				return false;
		}
	}

	/*
		this class handles the actions that have to be taken
		when the user double-clicks with the mouse on one of
		the elements of the list which is given as parameter
	*/
	private class ListActionListener implements ActionListener {
		
		List list;

		public ListActionListener(List list) {
			this.list = list;
		}

		public void actionPerformed(ActionEvent e) {
			String fileName = list.getItem(list.getSelectedIndex());

			if (fileName.equals("..") == true)
				go(curPath.getParent(), 'b');
			else if (fileName.charAt(0) == '[') {
				selections.addElement(new Integer(list.getSelectedIndex()));
				go(curPath.toString() + "\\" + fileName.substring(1, fileName.length()-1), 'f');
			}
		}
	}

	/*
		this class handles the actions that have to be taken
		when the user hits ENTER or BACKSPACE while browsing
		through the list which is given as parameter
	*/
	private class ListKeyListener extends KeyAdapter {

		List list;

		public ListKeyListener(List list) {
			this.list = list;
		}

		public void keyReleased(KeyEvent e) {
			String fileName = list.getItem(list.getSelectedIndex());

			if (e.getKeyCode() == KeyEvent.VK_ENTER)
				if (fileName.equals("..") == true)
					go(curPath.getParent(), 'b');
				else if (fileName.charAt(0) == '[') {
					selections.addElement(new Integer(list.getSelectedIndex()));
					go(curPath.toString() + "\\" + fileName.substring(1, fileName.length()-1), 'f');
				}
			if (e.getKeyCode() == KeyEvent.VK_BACK_SPACE)
				if (!(curPath.toString().equals("C:\\")))
					go(curPath.getParent().toString(), 'b');
		}
	}
}
