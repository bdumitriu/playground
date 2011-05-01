/**
 * 
 */
package gw.storage.javasvn;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.ISVNReporter;
import org.tmatesoft.svn.core.io.ISVNReporterBaton;

/**
 * A custom implementation of ISVNReporterBaton.
 * 
 * @author Bogdan Dumitriu
 */
public class SVNRAReporterBaton implements ISVNReporterBaton
{
    private PathTree _workingCopy;
    private String _repositoryURL;

    public SVNRAReporterBaton(PathTree workingCopy, String repositoryURL)
    {
        _workingCopy = workingCopy;
        _repositoryURL = repositoryURL;
    }

    public void report(ISVNReporter reporter) throws SVNException
    {
        _workingCopy.report(reporter, _repositoryURL);
    }

}
