
/**
 * Worker class used to set up a fresh MySQL installation.
 */
public class MySQLWorker extends Worker
{
	/**
	 * @param pathToMySQL the path to the directory where mysql is installed (i.e., if you append bin/ to this
	 *	it should be the directory where all the mysql executables are installed)
	 */
	public MySQLWorker(String pathToMySQL)
	{
		if (pathToMySQL.endsWith(System.getProperty("file.separator")))
		{
			this.path = pathToMySQL;
		}
		else
		{
			this.path = pathToMySQL + System.getProperty("file.separator");
		}
	}

	public void perform() throws Exception
	{
		StringBuilder sb = new StringBuilder(100);
		sb.append("\"");
		sb.append(path);
		sb.append("bin\\mysqld\" --install MySQL --defaults-file=\"");
		sb.append(path);
		sb.append("my-medium.ini\"");

		Process installServiceProc = Runtime.getRuntime().exec(sb.toString());

		if (installServiceProc.waitFor() != 0)
		{
			throw new Exception("There was some error while trying to run this command:\n" + sb.toString());
		}

		setChanged();
		notifyObservers(new ObserverEvent(getWorkerNr(), 50));

		sb.delete(0, sb.length());
		sb.append("NET START MySQL");

		Process startServiceProc = Runtime.getRuntime().exec(sb.toString());

		if (startServiceProc.waitFor() != 0)
		{
			throw new Exception("There was some error while trying to run this command:\n" + sb.toString());
		}

		setChanged();
		notifyObservers(new ObserverEvent(getWorkerNr(), 100));
	}

	private String path;
}
