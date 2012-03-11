package org.ffplanner;

import javax.ejb.EJB;
import javax.jws.WebService;

import org.ffplanner.bean.MovieBundleEJB;
import org.ffplanner.bean.MovieEJB;
import org.ffplanner.bean.ShowingEJB;
import org.ffplanner.scripts.TiffDownloader;

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
        new TiffDownloader().workYourMagic(showingEJB, movieEJB, movieBundleEJB);
    }
}
