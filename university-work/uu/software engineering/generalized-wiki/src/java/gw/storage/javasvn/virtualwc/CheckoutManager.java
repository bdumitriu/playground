package gw.storage.javasvn.virtualwc;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.pathtree.NodeNotFoundException;
import gw.storage.javasvn.pathtree.PathNode;
import gw.storage.javasvn.pathtree.RootNode;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.ISVNReporter;
import org.tmatesoft.svn.core.io.ISVNReporterBaton;
import org.tmatesoft.svn.core.io.SVNRepository;

public class CheckoutManager
{
	private SVNRepository repository;
	private RootNode root; 
	
	public CheckoutManager(SVNRepository repository, RootNode root)
	{
		this.repository = repository;
		this.root = root;
	}
	
	private String formatCheckoutPath(Path path)
	{
		String checkoutPath = path.toString();
		
		if ( checkoutPath == "/" )
			checkoutPath = null;
		else
			checkoutPath = checkoutPath.substring(1);
		
		return checkoutPath;
	}
	
	public void update(Path target, boolean recursive) throws StorageException
	{
		String checkoutPath = formatCheckoutPath(target);
		
		CheckoutEditor editor = new CheckoutEditor(root);
		
		try 
		{
			long latestRev = repository.getLatestRevision();
			ISVNReporterBaton baton = new ReporterBaton(root.getNode(target));
		
			repository.update(latestRev, checkoutPath, recursive,
					baton, editor);
		}
		catch ( SVNException e )
		{
			throw new StorageException(e);			
		}
		catch ( NodeNotFoundException e )
		{
			// if this happens ensurePathExists didn't do it's work
			assert false;
		}
	}
	
	public class ReporterBaton implements ISVNReporterBaton
	{
		private PathNode target;
		
		public ReporterBaton(PathNode target)
		{
			this.target = target;
		}
	
		public void report(ISVNReporter reporter) throws SVNException
		{
			//target.report();
			reporter.finishReport();
		}
	}
}
