package gw.query;

import gw.storage.*;

import java.io.*;
import java.util.*;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.*;
import org.apache.lucene.document.*;

/**
 * IndexUpdater is (obviously) the index updater. For each handler registered in
 * the IndexUpdater (currently this is _badly_ done in the constructor), we add
 * the fields into the index provided by these handlers. Each document that is
 * added to the index is passed to any of these handlers, so the handlers first
 * add their field to these documents. Finally this document is added to the
 * index. Hereafter we can finally start querying the index. Of course the index
 * updater can also delete and update the files in the index.
 */
public class IndexUpdater {
    private Vector handlers;

    private Storage _storage;

    /**
     * Create a new IndexUpdater using the given storage. The storage is needed
     * so we can read files from the wiki. In this constructor also the handlers
     * that add fields to the index are registered.
     * 
     * @param storage
     *            The storage with which we can read and write files
     */
    public IndexUpdater(Storage storage) {
        _storage = storage;
        handlers = new Vector();
        handlers.add(new TopicSearch());
        handlers.add(new ContentSearch());
        handlers.add(new LinkSearch());
        handlers.add(new ModifiedSearch());
    }

    /**
     * Add a file to the index. For this file, call all registered handlers, and
     * let them put their required fields into the document. In the end, add
     * this document to the index.
     * 
     * @param file
     *            The file that has to be put into the index
     * @return A message with information about the given file.
     */
    public String addFile(String file) throws IOException {
        if (file == null || file.equals("")) {
            return "";
        }

        try {
            String result = "";
            Document doc = createNewDocument(file);

            result = addFields(file, doc);

            addDocumentToIndex(doc);

            return result + file + "\n";
        } catch (StorageException e) {
            return file + " caused an error while getting its status: " + e;
        }

    }

    /**
     * Deletes the file from index for which the path is equal to the input.
     * 
     * @param file
     *            Filepath for the file to delete.
     * @throws IOException
     *             There was an exception trying to open up the index.
     */
    public void deleteFile(String file) throws IOException {
        Term term = new Term("path", file);
        IndexReader reader = IndexReader.open(Search.indexPath);
        reader.delete(term);
        reader.close();
    }

    /**
     * Update a file in the index. This can only be done by first removing this
     * file, and afterwards adding this file again.
     * 
     * @param file
     *            File to be updated throws IOException received from addFile
     *            and deleteFile
     */
    public void updateFile(String file) throws IOException {
        deleteFile(file);
        addFile(file);
    }

    /**
     * Finds out whether the specified file is a directory or not.
     * 
     * @param file
     *            File to look for
     * @throws StorageException
     *             there occured a StorageException while getting the status of
     *             the file from storage.
     */
    public boolean isDirectory(String file) throws StorageException {
        Map status = _storage.getStatus(file);
        StorageStatusMessage msg = (StorageStatusMessage) status.get(file);
        return msg.isDirectory();
    }

    /**
     * Create a new document which already contains the basic information which
     * is the path location for the file.
     * 
     * @param file
     *            Location of the file for which we are creating the document
     */
    private Document createNewDocument(String file) {
        Document doc = new Document();
        doc.add(Field.Keyword("path", file));
        return doc;
    }

    /**
     * Adds all fields created by the update handlers to the document for the
     * specified file.
     * 
     * @param file
     *            File to create the fields for.
     * @param doc
     *            The document in which to add the fields
     * @return String with some info regarding the file just indexed.
     */
    private String addFields(String file, Document doc) throws IOException, StorageException {
        boolean isDir = isDirectory(file);
        UpdateHandler handler;
        for (int i = 0; i < handlers.size(); i++) {
            handler = (UpdateHandler) handlers.get(i);
            handler.update(_storage, file, isDir, doc);
        }

        if (isDir) {
            return "Directory: ";
        } else {
            return "File: ";
        }
    }

    /**
     * Adds the specified document to the index.
     * 
     * @param doc
     *            The document to add to the index
     * @throws IOException
     *             there occured an i/o error while trying to add the document
     *             to the index
     */
    private void addDocumentToIndex(Document doc) throws IOException {
        IndexWriter writer = new IndexWriter(Search.indexPath, new StandardAnalyzer(), false);
        writer.addDocument(doc);
        writer.close();
    }

}
