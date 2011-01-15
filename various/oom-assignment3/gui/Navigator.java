package ass3.gui;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Stack;

import javax.swing.AbstractListModel;
import javax.swing.JList;

/**
 * A component based on JList that can be used to navigate through a directory
 * hierarchy.
 *
 * @author bdumitriu
 */
public class Navigator extends JList {

	private static final long serialVersionUID = 1L;

	/** the starting directory (can't go higher when here) */
	private File startDir;

	/**
	 * the parent directory of the currently displayed directory; needed when
	 * going back up in the directory hierarchy
	 */
	private File parentDir;

	/** the list of files currently displayed */
	private File[] currentFiles;

	/**
	 * when navigating, maintain a stack of positions so that when we go back up
	 * we can reset the selected index in the list to what it used to before we
	 * went down
	 */
	private Stack<Integer> positions = new Stack<Integer>();

	/**
	 * if at the top of the hierarchy (i.e., at startDir), don't display the
	 * ".." entry that is usually used to go up
	 */
	private boolean atTop;

	/** the custom list model we use to control what the JList displays */
	private MyListModel myListModel;

	public Navigator(String startDir) {
		this.startDir = this.parentDir = new File(startDir);

		myListModel = new MyListModel();
		setModel(myListModel);

		changeDir(this.startDir, false);

		addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					elementSelected();
				}
			}
		});

		addKeyListener(new KeyAdapter() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == '\n') {
					elementSelected();
				}
			}
		});
	}

	private void elementSelected() {
		if (!atTop && getSelectedIndex() == 0) {
			changeDir(parentDir.getParentFile(), true);
		} else {
			File file = currentFiles[getSelectedIndex() - (atTop ? 0 : 1)];
			if (file.isDirectory()) {
				changeDir(file, false);
			}
		}
	}

	private void changeDir(File dir, boolean goingUp) {
		atTop = dir.equals(startDir);
		this.parentDir = dir;
		currentFiles = dir.listFiles();
		Arrays.sort(currentFiles, new Comparator<File>() {
			@Override
			public int compare(File o1, File o2) {
				if (o1.isFile() && o2.isFile()
						|| o1.isDirectory() && o2.isDirectory()) {
					return o1.compareTo(o2);
				} else if (o1.isFile() && o2.isDirectory()) {
					return 1;
				} else {
					return -1;
				}
			}
		});
		if (!goingUp) {
			positions.push(getSelectedIndex());
		}
		myListModel.update();
		if (goingUp) {
			setSelectedIndex(positions.pop());
		} else {
			setSelectedIndex(0);
		}
		
	}

	private final class MyListModel extends AbstractListModel {
		private static final long serialVersionUID = 1L;

		@Override
		public Object getElementAt(int index) {
			if (!atTop && index == 0) {
				return "..";
			} else {
				return _getElementAt(index - (atTop ? 0 : 1));
			}
		}

		private Object _getElementAt(int index) {
			File f = currentFiles[index];
			if (f.isFile()) {
				return f.getName();
			} else {
				return "[" + f.getName() + "]";
			}
		}

		@Override
		public int getSize() {
			return atTop ? currentFiles.length : currentFiles.length + 1;
		}

		public void update() {
			fireContentsChanged(this, 0, currentFiles.length - 1);
		}
	}
}
