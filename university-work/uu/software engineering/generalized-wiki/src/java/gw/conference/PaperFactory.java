package gw.conference;

import gw.GwConstants;
import gw.ServletUtilities;
import gw.conference.exceptions.InvalidAuthorException;
import gw.conference.exceptions.InvalidConferenceException;
import gw.conference.exceptions.InvalidPaperException;
import gw.conference.exceptions.PaperExistsException;
import gw.conference.ui.ConferencePages;
import gw.render.parsers.ParseException;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.users.User;

import java.io.IOException;
import java.util.Map;

public class PaperFactory {    
    public static Paper openPaper(Storage storage, String contextPath, String paperPath)
        throws InvalidPaperException, StorageException, InvalidConferenceException {
        String pathComponents[];
        String conferencePath, wikiTitle;
        Conference conference;
        
        pathComponents = ServletUtilities.splitPathInArray(paperPath);
        int length = pathComponents.length;

        if (!pathComponents[length-2].equals(Constants.PAPERS_PREFIX))
            throw new InvalidPaperException("Path " + paperPath + " is not a valid paper path.");
        
        conferencePath = "/" + ServletUtilities.joinPathFromArray(pathComponents, length-2);
        wikiTitle = pathComponents[length-1];
        
        conference = ConferenceFactory.openConference(storage, contextPath, conferencePath);
        
        return openPaper(conference, wikiTitle);
    }
    
    public static Paper openPaper(Conference conference, String wikiTitle)
            throws InvalidPaperException, StorageException {

        Storage storage = conference.getStorage();
        
        String paperRoot = conference.getPath() + Constants.PAPERS_PREFIX + "/" + wikiTitle + "/";
        String indexPath = paperRoot + GwConstants.INDEX_PAGE_NAME;
        
        if (!storage.fileExists(indexPath)) throw new InvalidPaperException("No such paper " + wikiTitle +
            " in the conference " + conference.getFullTitle() + ".");
                
        return new Paper(conference, paperRoot, wikiTitle);
    }
    
    /**
     * Submits a paper for the current user/author.
     * @param properties The additional properties for this file, e.g. "title"
     * @throws ParseException 
     */
    public static Paper submitPaper(Conference conference, User user, String wikiTitle, String fullTitle, Map<String,String> properties) 
        throws StorageException, IOException, PaperExistsException, InvalidAuthorException, ParseException {  
        
        // Confirm user is an author
        String authorPage = conference.getPath() + Constants.AUTHORS_PREFIX + "/" + user.getId();
        if(!conference.getStorage().fileExists(authorPage))
            throw new InvalidAuthorException(user.getId());
        
        // Determine properties of this paper
        Paper paper = createInfrastructure(conference, wikiTitle, fullTitle, user.getId(), properties);
        
        
        // Set access rights
        new ACLManager(conference).setPaperRights(paper);
        
        return paper;
    }


    private static Paper createInfrastructure(Conference conference, String wikiTitle, String fullTitle, String author, Map<String, String> properties) throws StorageException, PaperExistsException, IOException, ParseException {

        Storage storage = conference.getStorage();
        String paperPath = conference.getPath() + Constants.PAPERS_PREFIX + "/" + wikiTitle + "/";
        String paperReviewsPath = paperPath + Constants.REVIEWS_PREFIX;
        String paperIndexFile = paperPath + GwConstants.INDEX_PAGE_NAME;
        String ratingFile = conference.getPath() + Constants.PAPER_RATINGS;

        storage.ensurePathExists(paperPath);
        storage.ensurePathExists(paperReviewsPath);
        
        if(storage.fileExists(paperIndexFile))
            throw new PaperExistsException(wikiTitle);
        
        Paper paper = new Paper(conference, paperPath);        
        paper.setTitle(fullTitle);
        paper.setAuthors(author); // TODO: Use Constants.AUTHORS field from properties map for this!      
        
        // Write paper home page
        String paperPage = ConferencePages.getNewPaperPage(paper, author, properties);

        ConferencePages.write(storage, paperIndexFile, paperPage);

        // Write add rating page
        String ratingPage = ConferencePages.getAddRatingPage(paper);
        ConferencePages.write(storage, paperPath + Constants.ADD_RATING_FILE, ratingPage);
        
        // TODO: Write discussion page
        // TODO: Write abstract page??
        
        // Write assign reviewer page
        String assignReviewer = ConferencePages.getAssignReviewerPage(paper);
        ConferencePages.write(storage, paperPath + Constants.ASSIGN_REVIEWER_FILE, assignReviewer);
        
        PaperRatings pr = new PaperRatings();
        pr.construct(storage.getFile(ratingFile));
        pr.addPaper(fullTitle);
        
        ConferencePages.writeRatingFile(pr,storage, ratingFile);

        
        return paper;
    }
}
