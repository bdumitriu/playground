package gw.conference.actions;

import gw.conference.*;
import gw.conference.exceptions.*;
import gw.storage.StorageException;

import java.io.IOException;

/** Updates the state of a given conference (using the deadlines of the conference). */
public class UpdateState extends ConferenceServlet {
    private static final long serialVersionUID = -8514168328544219702L;

    @Override
    public void doRequest(ActionContext action) throws ConferenceException,
           StorageException, IOException {
        
        Conference conference = action.openConferenceHere();
        conference.updateState();
        conference.updateACL();
        
        action.writeMessage("State successfully updated."); // (If no exceptions thrown)
    }
}
