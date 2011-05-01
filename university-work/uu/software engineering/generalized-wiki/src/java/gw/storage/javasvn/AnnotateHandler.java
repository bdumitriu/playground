package gw.storage.javasvn;

import gw.storage.StorageBlameLine;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

import org.tmatesoft.svn.core.wc.ISVNAnnotateHandler;

public class AnnotateHandler implements ISVNAnnotateHandler
{
	private ArrayList<StorageBlameLine> _lines = 
		new ArrayList<StorageBlameLine>();
	
	/** @see org.tmatesoft.svn.core.wc.ISVNAnnotateHandler#handleLine(java.util.Date, long, java.lang.String, java.lang.String)*/
	public void handleLine(Date date, long revision, String author, String line)
	{
		_lines.add(new StorageBlameLine(date, revision, author, line));
	}
	
	/**
	 * This function can be called <b>after</b> this class has been
	 * used to retrieve the status by doAnnotate().
	 * @see org.tmatesoft.svn.core.wc.SVNLogClient#doAnnotate(org.tmatesoft.svn.core.SVNURL, org.tmatesoft.svn.core.wc.SVNRevision, org.tmatesoft.svn.core.wc.SVNRevision, org.tmatesoft.svn.core.wc.SVNRevision, org.tmatesoft.svn.core.wc.ISVNAnnotateHandler)
	 * @return The iterator to the annotated first line
	 * (if the file contains any lines)
	 */
	public Iterator<StorageBlameLine> getResultIterator()
	{
		return _lines.iterator();
	}

}
