package gw.query;

import gw.*;
import gw.storage.*;

import java.util.Iterator;
import java.util.Map;

// Servlets
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.IndexWriter;

//To do some little benchmarking
import java.util.Calendar;

/**
 * RebuildSearchIndex is the servlet that is able to create a new Index. This
 * new index * will be the main search index. If a previous index exists, it is
 * just thrown away. There are many reasons to really want to do this: the old
 * index might not contain some fields that are supported now, so you want to
 * create a new index. Also, if the server has been running correctly,
 * rebuildIndex needs not be called at all: the updates can be handled completly
 * automatically through the incremental updates (see * the EventHandler).
 * 
 * @see gw.query.EventHandler
 */
public class RebuildSearchIndex extends GwServlet {

    /**
     * In the case of RebuildSearchIndex, no parameters are required. We really
     * want to forward everything to the same action. So it is just a dispatcher
     * to the doRequest method.
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        doRequest(request, response);
    }

    /**
     * In the case of RebuildSearchIndex, no parameters are required. We really
     * want to forward everything to the same action. So it is just a dispatcher
     * to the doRequest method.
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        doRequest(request, response);
    }

    /**
     * doRequest initiates the building of the index. It also keeps track of the
     * time, to be able to do a small time measurement.
     * 
     * @param request
     *            The HttpServletRequest that is the request that initiates this
     *            servlet.
     * @param response
     *            The response we want to output to.
     * @throws IOException
     *             throws IOException when it receives an Exception from the
     *             printwriter to the output.
     */
    public void doRequest(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        long start = Calendar.getInstance().getTime().getTime();

        // Obtain a storage interface and pass that to createIndex.
        PrintWriter cout = response.getWriter();
        response.setContentType(GwConstants.XHTML_MIME_TYPE);
        cout.println("Indexing all documents. Please wait...\n");
        cout.flush();

        // initialize the storage object
        GwSessionContext gsc = getSessionContext(request);
        Storage storage = gsc.getUnsecuredStorage();

        // Start indexing
        String createIndex = createIndex(Search.indexPath, storage);

        // Finished indexing: take the end time for the benchmark
        long time = Calendar.getInstance().getTime().getTime() - start;

        cout.println("<b>Indexing finished.</b><br>Time elapsed: " + time + " ms\n");
        cout.println("<pre>" + createIndex + "</pre>");
    }

    /**
     * Creates in an index on the specified location. The output is just a list
     * of files that are indexed now.
     */
    private String createIndex(String indexPath, Storage storage) {
        String result = "";
        try {
            result = touchIndex(indexPath);
            Map status = storage.getStatus("/", true);
            Iterator listing = status.keySet().iterator();
            IndexUpdater updater = new IndexUpdater(storage);
            while (listing.hasNext()) {
                // The file string was a key in the status Map
                String file = (String) listing.next();
                result = result + updater.addFile(file);
            }
            return result;
        } catch (StorageException e) {
            return result + "\nError with the storage layer. " + e.getMessage();
        } catch (IOException e) {
            return result + "\nThere was an exception: " + e;
        }
    }

    /**
     * Touches the index: make sure an index exists at the specified location
     * 
     * @param indexPath
     *            Location of the index to be touched
     * @throws IOException
     *             there occured an IOException when opening the index
     */
    private String touchIndex(String indexPath) throws IOException {
        IndexWriter writer = new IndexWriter(indexPath, new StandardAnalyzer(), true);
        writer.close();
        return "Index located at: " + indexPath + "\n";
    }
}
