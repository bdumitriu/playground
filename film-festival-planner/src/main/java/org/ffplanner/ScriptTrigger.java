package org.ffplanner;

import org.ffplanner.bean.MovieBean;
import org.ffplanner.bean.MovieBundleBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.scripts.TiffDownloader;

import javax.ejb.EJB;
import javax.jws.WebService;

/**
 * @author Bogdan Dumitriu
 */
@WebService
public class ScriptTrigger {

    @EJB
    private ShowingBean showingBean;

    @EJB
    private MovieBean movieBean;

    @EJB
    private MovieBundleBean movieBundleBean;

    public void triggerTiffDownload() {
        new TiffDownloader(showingBean).fillDatabaseFromSite(movieBean, movieBundleBean);
    }
}
