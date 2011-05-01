package gw.storage.javasvn.virtualwc;

import java.util.Date;

import org.tmatesoft.svn.core.SVNProperty;
import org.tmatesoft.svn.core.internal.util.SVNTimeUtil;

import gw.storage.StorageException;
import gw.storage.StorageStatusMessage;
import gw.storage.javasvn.pathtree.NodeData;
import gw.storage.javasvn.pathtree.PathNode;

public class VirtualWCStorageStatusMessage extends StorageStatusMessage
{
	private boolean isDirectory;
	private long revision;
	
	private String lastAuthor;
	private long lastRevision;
	private Date lastDate;
	
	private boolean isAdded;
	private boolean isCopied;
	private boolean isDeleted;
	private boolean isModified;
	
	public VirtualWCStorageStatusMessage(PathNode node) throws StorageException
	{
		isDirectory = node.isDir();
		revision = node.getRoot().getRevision();
		
		NodeData data = node.getData();
		isAdded = data.isAdd();
		isCopied = data.isCopy();
		isDeleted = data.isDelete();
		isModified = data.isModFile() || data.isModProperty();
		
		lastAuthor = data.getProperty(SVNProperty.LAST_AUTHOR);
		lastDate = SVNTimeUtil.parseDate(data.getProperty(SVNProperty.COMMITTED_DATE));
		lastRevision = Long.parseLong(data.getProperty(SVNProperty.COMMITTED_REVISION));
		
		if ( node.getData().isAdd() )
			lastRevision = -1;
	}

	@Override
	public Date getLastChangedDate()
	{
		return lastDate;
	}

	@Override
	public long getLastChangedRevision()
	{
		return lastRevision;
	}

	@Override
	public String getLastCommitAuthor()
	{
		return lastAuthor;
	}

	@Override
	public long getRevision()
	{
		return revision;
	}

	@Override
	public boolean isAdded()
	{
		return isAdded;
	}

	@Override
	public boolean isCopied()
	{
		return isCopied;
	}

	@Override
	public boolean isDeleted()
	{
		return isDeleted;
	}

	@Override
	public boolean isModified()
	{
		return isModified;
	}

	@Override
	public boolean isDirectory()
	{
		return isDirectory;
	}

	@Override
	public boolean isFile()
	{
		return !isDirectory;
	}

}
