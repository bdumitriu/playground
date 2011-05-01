package gw.conference;

//import java.io.*;
import java.io.IOException;
import java.util.*;

import gw.GwConstants;
import gw.conference.exceptions.*;

import gw.conference.ui.ConferencePages;
import gw.storage.*;
import gw.users.*;

/** 
 * Represents a conference. 
 * Allows one to get/set properties of the conference, and add papers, etc.
 * Be sure to call updateACL() when appropriate after changing such properties.
 * @see #updateACL
 * */
public class Conference {
    private final ConferenceDataAdapter _data;
    private final String _contextPath;
	
	/** 
	 * Creates a new instance of the ConferenceManager class. Use the
	 * ConferenceManagerFactory class to create such an instance.
     * @see ConferenceFactory
	 */
	protected Conference(Storage storage, String contextPath, String path) throws StorageException {
		_data = new ConferenceDataAdapter(storage, path);
        _contextPath = contextPath;
	}
	
	/*
	 * ACCESSORS
	 */
    
    /** Returns the associated path, with an ending slash. */
    public String getPath() {
        return _data.getPath();
    }
    public String getContextPath() {
        return _contextPath;
    }    
    public Storage getStorage() {
        return _data.getStorage();
    }
    
    public String getChairperson() {
        return _data.get(Constants.CHAIRPERSON);
	}	
	public void setChairperson(String value) throws StorageException {
        _data.set(Constants.CHAIRPERSON, value);
	}
    
    public String[] getPCMembers() {
        String property = _data.get(Constants.PC_MEMBERS);
        if(property == null) return new String[0];
        return property.split(",");
    }
    
    public void setPCMembers(List<String> usernames) throws StorageException {
        _data.setList(Constants.PC_MEMBERS, usernames);
    }

    public String getFullTitle() {
        return _data.get(Constants.FULL_TITLE);
    }   
    public void setFullTitle(String value) throws StorageException {
        _data.set(Constants.FULL_TITLE, value);
    }
	
	/** Returns the current state of the conference, or null if unavailable/invalid. */
    public ConferenceState getState() {
		try {
		    return ConferenceState.valueOf(_data.get(Constants.STATE));
        } catch (Exception x) {
            return null;
        }
	}
    
    /* Sets the state of this conference. Be sure to call updateACL(). */
	public void setState(ConferenceState value) throws StorageException {
        _data.set(Constants.STATE, value.toString());        
	}
	
    public ConferenceDate getPaperSubmitDeadline() {
		return _data.getDate(Constants.PAPER_SUBMIT_DEADLINE);
	}
	public ConferenceDate getReviewerAssignDeadline() {
		return _data.getDate(Constants.REVIEWER_ASSIGN_DEADLINE);
	}
	public ConferenceDate getReviewSubmitDeadline() {
		return _data.getDate(Constants.REVIEW_SUBMIT_DEADLINE);
	}
	public ConferenceDate getDiscussionSubmitDeadline() {
		return _data.getDate(Constants.DISCUSSION_SUBMIT_DEADLINE);
	}
	public ConferenceDate getFinalDeadline() {
		return _data.getDate(Constants.FINAL_DEADLINE);
	}
	
	public void setPaperSubmitDeadline(ConferenceDate value) throws StorageException {
        _data.setDate(Constants.PAPER_SUBMIT_DEADLINE, value);
	}
	public void setReviewerAssignDeadline(ConferenceDate value) throws StorageException {
        _data.setDate(Constants.REVIEWER_ASSIGN_DEADLINE, value);
	}
	public void setReviewSubmitDeadline(ConferenceDate value) throws StorageException {
        _data.setDate(Constants.REVIEW_SUBMIT_DEADLINE, value);
	}
	public void setDiscussionSubmitDeadline(ConferenceDate value) throws StorageException {
        _data.setDate(Constants.DISCUSSION_SUBMIT_DEADLINE, value);
	}
	public void setFinalDeadline(ConferenceDate value) throws StorageException {
        _data.setDate(Constants.FINAL_DEADLINE, value);
	}
    
    public List<Paper> getPapers() throws StorageException {
        Iterator<String> papers = getStorage().getDirListing(getPath() + Constants.PAPERS_PREFIX);
        List<Paper> result = new ArrayList<Paper>();
        
        while(papers.hasNext()) {
            String path = papers.next();
            try {
                result.add(PaperFactory.openPaper(this, Paper.getWikiTitle(path)));
            } catch(InvalidPaperException x) {
                System.out.println("Paper " + path + " is invalid."); // TODO: Write to system log/warnings (once we have one)                
            }
        }
        
        return result;
    }

	/*
	 * PUBLIC METHODS
	 */
    
    /** 
     * Sets all the properties from the given map to this conference.
     * @throws InvalidPropertyException Thrown if an invalid property NAME is specified
     */
    public void addProperties(Map<String,String> properties) throws InvalidPropertyException, StorageException {
        // From the given map, set all the valid properties using _data (a.o. REVIEWER_ASSIGN_DEADLINE, etc.)
        for(Map.Entry<String,String> entry : properties.entrySet()) {
            String property = entry.getKey(), value = entry.getValue(); 
            
            if(!isValidProperty(property)) throw new InvalidPropertyException(property);
            
            _data.set(property, value);
        }
        
    }
    
    public boolean isValidProperty(String property) {
        for(String valid : Constants.CONFERENCE_PROPERTIES)
            if(valid.equals(property)) return true;
        
        return false;
    }
	
	/** Checks if this conference is a valid and existing conference */
	public boolean isValid() throws StorageException {
		return getState() != null;
	}
	
	/** 
	 * @throws AuthorExistsException
	 * @throws StorageException
	 * @throws NoSuchUserException
	 */
	public void registerAuthor(User user, String fullTitle, Map<String,String> fields) 
		throws AuthorExistsException, StorageException, IOException {
		
		String name = user.getId();
        String authorPath = getPath() + Constants.AUTHORS_PREFIX + "/" + name + "/";
    	String authorFile = authorPath + GwConstants.INDEX_PAGE_NAME;
        Storage storage = _data.getStorage();
        
        storage.ensurePathExists(authorPath);
    	
        if(storage.fileExists(authorFile))
        	throw new AuthorExistsException(name);
        
        String authorPage = ConferencePages.getNewAuthorPage(this, fullTitle, fields);
        ConferencePages.write(storage, authorFile, authorPage);
	}
	
	/** Updates the conference state using the deadlines, and updates the ACL if necessary. */
	public void updateState() throws StorageException {
		Calendar now = Calendar.getInstance();
		ConferenceState state;
        
		// TODO: Test this code, current unit test fails		
        
		if(now.compareTo(getPaperSubmitDeadline()) <= 0) {
		    state = ConferenceState.BEFORE_PAPER_DEADLINE;
		} else if(now.compareTo(getReviewerAssignDeadline()) <= 0) {
            state = ConferenceState.BEFORE_REVIEWER_ASSIGN_DEADLINE;
		} else if(now.compareTo(getReviewSubmitDeadline()) <= 0) {
            state = ConferenceState.BEFORE_REVIEW_DEADLINE;
		} else if(now.compareTo(getDiscussionSubmitDeadline()) <= 0) {
            state = ConferenceState.BEFORE_DISCUSSION_DEADLINE;
		} else if(now.compareTo(getFinalDeadline()) <= 0) {
            state = ConferenceState.BEFORE_FINAL_DEADLINE;
		} else {
            state = ConferenceState.FINISHED;
        }
        
        if(!getState().equals(state)) {
            setState(state);
            updateACL();
        }
	}
    
    public void updateACL() throws StorageException {
        ACLManager acl = new ACLManager(this);
        
        for(Paper paper : getPapers())
            acl.setPaperRights(paper);
    }
}