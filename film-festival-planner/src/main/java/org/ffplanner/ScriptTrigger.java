package org.ffplanner;

import org.ffplanner.bean.MovieBundleEJB;
import org.ffplanner.bean.MovieEJB;
import org.ffplanner.bean.ShowingEJB;
import org.ffplanner.scripts.TiffDownloader;

import javax.ejb.EJB;
import javax.jws.WebService;

/**
 * @author Bogdan Dumitriu
 */
@WebService
public class ScriptTrigger {

    @EJB
    private ShowingEJB showingEJB;

    @EJB
    private MovieEJB movieEJB;

    @EJB
    private MovieBundleEJB movieBundleEJB;

    public void triggerTiffDownload() {
        new TiffDownloader(showingEJB).fillDatabaseFromSite(movieEJB, movieBundleEJB);
    }
}
