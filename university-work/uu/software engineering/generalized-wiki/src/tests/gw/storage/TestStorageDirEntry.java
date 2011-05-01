package gw.storage;

import java.util.Date;

import gw.storage.StorageDirEntry;

public class TestStorageDirEntry extends StorageDirEntry {
	
	public TestStorageDirEntry(Date date, long revision,
			String author,boolean isFile, boolean isDir)
	{
		this.date = date;
		this.revision = revision;
		this.author = author;
		this.isFile = isFile;
		this.isDir = isDir;
	}
	
	private Date date;
	private long revision;
	private String author;
	private boolean isFile;
	private boolean isDir;

	@Override
	public Date getLastChangedDate()
	{
		return date;
	}

	@Override
	public long getLastChangedRevision()
	{
		return revision;
	}

	@Override
	public String getLastChangedAuthor()
	{
		return author;
	}

	@Override
	public boolean isDirectory()
	{
		return isDir;
	}

	@Override
	public boolean isFile() 
	{
		return isFile;
	}

}
