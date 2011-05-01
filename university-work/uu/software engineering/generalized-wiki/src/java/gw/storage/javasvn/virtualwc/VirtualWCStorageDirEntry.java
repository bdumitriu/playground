package gw.storage.javasvn.virtualwc;

import java.util.Date;
import org.tmatesoft.svn.core.SVNProperty;
import org.tmatesoft.svn.core.internal.util.SVNTimeUtil;

import gw.storage.StorageDirEntry;
import gw.storage.StorageException;
import gw.storage.javasvn.pathtree.NodeData;
import gw.storage.javasvn.pathtree.PathNode;

public class VirtualWCStorageDirEntry extends StorageDirEntry
{
	private boolean isDirectory;
	private String lastAuthor;
	private long lastRevision;
	private Date lastDate;
	
	public VirtualWCStorageDirEntry(PathNode node) throws StorageException
	{
		isDirectory = node.isDir();
		
		NodeData data = node.getData();
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
	public String getLastChangedAuthor()
	{
		return lastAuthor;
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
