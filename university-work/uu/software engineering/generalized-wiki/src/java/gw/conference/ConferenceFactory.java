package gw.conference;

import gw.GwConstants;
import gw.conference.exceptions.ConferenceExistsException;
import gw.conference.exceptions.InvalidConferenceException;
import gw.conference.ui.ConferencePages;
import gw.storage.Storage;
import gw.storage.StorageException;

import java.io.IOException;

public class ConferenceFactory {    
    public static Conference openConference(Storage storage, String contextPath, String path) 
        throws StorageException, InvalidConferenceException {
        
        Conference conference;      
        try {
            conference = new Conference(storage, contextPath, path);
        } catch(StorageException x) {
            throw new InvalidConferenceException("Conference does not exist or storage problem", x);
        }
        
        if(!conference.isValid())
            throw new InvalidConferenceException("Not a valid conference (has no state property set).");
        
        return conference;        
    }
    
	/** Creates a new conference in the given path. 
	 * 
	 * @param fullTitle The full title of the conference, will not be used as the WikiName.
	 * @throws StorageException
     * @throws ConferenceExistsException Thrown when the given path exists AND is already a valid conference.
     * @throws IOException 
	 * */
	public static Conference createNewConference(Storage storage, String path, String contextPath, String fullTitle, String chairPerson) 
		throws StorageException, ConferenceExistsException, IOException {	
        
		storage.ensurePathExists(path);
		
		Conference conference = new Conference(storage, contextPath, path);
          
        if(conference.isValid()) // A conference already exists here
            throw new ConferenceExistsException();
		
		// Set properties of the new conference
		conference.setState(Constants.INITIAL_STATE);
        conference.setChairperson(chairPerson);
        conference.setFullTitle(fullTitle);
        
        createInfrastructure(storage, conference);
        
        if(!conference.isValid()) // Shouldn't happen
            throw new StorageException("Failed to store the new conference"); 
		
		return conference;
	}

    /** Creates directories and files for a new conference. */
    private static void createInfrastructure(Storage storage, Conference conference) throws StorageException, IOException {
        String path = conference.getPath();
        
        // "Welcome" page
        String welcome = ConferencePages.getConferenceWelcomePage(conference);
        ConferencePages.write(storage, path + GwConstants.INDEX_PAGE_NAME, welcome);
        
        // AddPaper form
        String paper = ConferencePages.getAddPaperPage(conference);
        ConferencePages.write(storage, path + Constants.ADD_PAPER_FILE, paper);

        // AddAuthor form
        String author = ConferencePages.getAddAuthorPage(conference);
        ConferencePages.write(storage, path + Constants.ADD_AUTHOR_FILE, author);
        
        // The rating xml file

        PaperRatings ratings = new PaperRatings();
        ConferencePages.writeRatingFile(ratings, storage, path + Constants.PAPER_RATINGS);

        
   }
}