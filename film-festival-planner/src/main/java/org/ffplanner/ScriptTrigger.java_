package org.ffplanner;

import org.ffplanner.bean.MovieBean;
import org.ffplanner.bean.MovieBundleBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.scripts.TiffDownloader;

import javax.inject.Inject;
import javax.jws.WebService;

/**
 * @author Bogdan Dumitriu
 */
@WebService
public class ScriptTrigger {

    @Inject
    private ShowingBean showingBean;

    @Inject
    private MovieBean movieBean;

    @Inject
    private MovieBundleBean movieBundleBean;

    public void triggerTiffDownload() {
        new TiffDownloader(showingBean).fillDatabaseFromSite(movieBean, movieBundleBean);
    }
}
