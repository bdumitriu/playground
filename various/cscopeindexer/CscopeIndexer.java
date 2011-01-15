package cscopeindexer;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

import gnu.getopt.Getopt;

public class CscopeIndexer {

	private String dirName = ".";
	private String dbFileName = "cscope.out";
	private String listFileName = "cscope.files";
	private boolean listOnly = false;
	private boolean recurse = false;
	private boolean verbose = false;

	public CscopeIndexer(String args[]) {
		Getopt g = new Getopt("CscopeIndexer", args, "lrvf:i:");
		int c;
		while ((c = g.getopt()) != -1) {
			switch(c) {
			case 'l':
				listOnly = true;
				break;
			case 'r':
				recurse = true;
				break;
			case 'v':
				verbose = true;
				break;
			case 'f':
				dbFileName = g.getOptarg();
				break;
			case 'i':
				listFileName = g.getOptarg();
				break;
			case '?':
				System.exit(1);
			default:
				System.out.println("getopt() returned " + c + "\n");
				System.exit(1);
			}
		}
		for (int i = g.getOptind(); i < args.length ; i++) {
			dirName = args[i];
		}
	}

	public void index() throws IOException, InterruptedException {
		if (verbose) {
			System.out.println("Creating list of files to index ...");
		}

		ArrayList<String> files = new ArrayList<String>();
		File dir = new File(dirName);
		if (recurse) {
			collectFiles(dir, files);
		} else {
			if (dir.isDirectory() && !dir.getPath().equals(".svn")) {
				String[] children = dir.list();
				for (int i = 0; i < children.length; i++) {
					File file = new File(dirName, children[i]);
					String absolutePath = file.getAbsolutePath();
					if (file.isFile()
							&& absolutePath.matches(".*\\.([chly](xx|pp)*|cc|hh)$")) {
						files.add(absolutePath);
					}
				}
			}
		}

		File listFile = new File(dir, listFileName);
		PrintWriter pw = new PrintWriter(listFile);
		for (String file : files) {
			pw.write(file + "\n");
		}
		pw.close();

		if (verbose) {
			System.out.println("Creating list of files to index ... done");
		}
		
		if (listOnly) {
			return;
		}

		if (verbose) {
			System.out.println("Indexing files ...");
		}

		//File dbFile = new File(dir, dbFileName);

		ProcessBuilder pb = new ProcessBuilder(
				"cmd", "/c", "cscope", "-b", "-i", listFile.getAbsolutePath());
		pb.directory(dir);
		Process p = pb.start();
		p.waitFor();

//		ProcessBuilder pb = new ProcessBuilder(
//				"cscope-indexer2.bat", listFile.getAbsolutePath());
//		pb.directory(dir);
//		Process p = pb.start();
//		p.waitFor();

		if (verbose) {
			System.out.println("Indexing files ... done");
		}
	}

	public static void collectFiles(File dir, ArrayList<String> files) {
		if (dir.isDirectory() && !dir.getPath().equals(".svn")) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				collectFiles(new File(dir, children[i]), files);
			}
		} else {
			String absolutePath = dir.getAbsolutePath();
			if (absolutePath.matches(".*\\.([chly](xx|pp)*|cc|hh)$")) {
				files.add(absolutePath);
			}
		}
	}

	public static void main(String[] args) {
		try {
			new CscopeIndexer(args).index();
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
	}
}
