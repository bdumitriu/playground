package chatAdmin;

import java.io.Serializable;

/**
 * This class is used to store information about the logs which the chat
 * server keeps.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0
 */
public class Log implements Serializable, Cloneable
{
	/*
	 * the name of the file in which the log is stored.
	 */
	private String fileName;

	/*
	 * the directory in which the fileName is stored.
	 */
	private String directory;

	/**
	 * Builds a new log object with the specified file. The file parameter
	 * should contain the absolute path of the file because directory info
	 * will be retrieved from it.
	 * <br><br>
	 * Calls to {@link #getFileName() getFileName()} and {@link
	 * #getDirectory() getDirectory()} will return the file part and
	 * the directory part respectively of the file parameter supplied here.
	 */
	public Log(String file)
	{
		this.fileName = file;
		directory = null;
	}

	/*
	 * Sets the name of the file in which the log is stored to fileName.
	 */
	private void setFileName(String fileName)
	{
		this.fileName = fileName;
	}

	/**
	 * Returns the name of the file in which the log is stored. No path
	 * information is provided here.
	 * <br><br>
	 * @see #getDirectory() getDirectory()
	 */
	public String getFileName()
	{
		return fileName;
	}

	/**
	 * Sets the name of the direcotry in which the log is stored to dir.
	 */
	public void setDirectory(String dir)
	{
		directory = dir;
	}

	/**
	 * Returns the name of the directory in which the log is stored. The
	 * result might be null if no directory info is supplied when this
	 * log object is built.
	 */
	public String getDirectory()
	{
		return directory;
	}
	
	/**
	 * Returns a copy of this object.
	 * <br><br>
	 */
	public Object clone()
	{
		Log clonedLog = new Log(this.fileName);
		clonedLog.setFileName(this.fileName);
		clonedLog.setDirectory(this.directory);
		return clonedLog;
	}
}
