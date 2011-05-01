package gw.storage;

import java.util.Date;

import gw.storage.StorageStatusMessage;

/**
 * This class is used to compare StorageStatusMessages
 * @author gideon
 * @see GetStatusTest
 */
public class TestStorageStatusMessage extends StorageStatusMessage
{
	public TestStorageStatusMessage(
			Date lastChangedDate,
			long lastChangedRevision,
			String lastCommitAuthor,
			boolean isFile,
			int statusType,
			long revision)
	{
		_lastChangedDate = lastChangedDate;
		_lastChangedRevision = lastChangedRevision;
		_lastCommitAuthor = lastCommitAuthor;
		_isFile = isFile;
		_statusType = statusType;
		_revision = revision;
	}
	
	private Date _lastChangedDate;
	private long _lastChangedRevision;
	private String _lastCommitAuthor;
	private boolean _isFile;
	private int _statusType;
	private long _revision;

	public class StatusType
	{
		public static final int NONE = 0;
		public static final int COPIED = 1;
		public static final int ADDED = 2;
		public static final int DELETED = 3;
		public static final int MODIFIED = 4;
	}

	public Date getLastChangedDate(){
		return _lastChangedDate;
	}

	public long getLastChangedRevision() {
		return _lastChangedRevision;
	}

	public String getLastCommitAuthor() {
		return _lastCommitAuthor;
	}

	public long getRevision() {
		return _revision;
	}

	public boolean isAdded() {
		return _statusType == StatusType.ADDED;
	}

	public boolean isCopied() {
		return _statusType == StatusType.COPIED;
	}

	public boolean isDeleted() {
		return _statusType == StatusType.DELETED;
	}

	public boolean isModified() {
		return _statusType == StatusType.MODIFIED;
	}

	public boolean isDirectory() {
		return !_isFile;
	}

	public boolean isFile() {
		return _isFile;
	}
}
