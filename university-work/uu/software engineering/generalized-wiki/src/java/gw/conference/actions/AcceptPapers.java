package gw.conference.actions;

import gw.conference.Conference;
import gw.conference.Paper;
import gw.conference.exceptions.ChairpersonOnlyException;
import gw.conference.exceptions.ConferenceException;
import gw.conference.exceptions.InvalidServletAccessException;
import gw.render.parsers.ParseException;
import gw.storage.StorageException;

import java.io.IOException;

import org.jdom.JDOMException;

public class AcceptPapers extends ConferenceServlet {
    private static final long serialVersionUID = -2972118221429276013L;

    @Override
    public void doRequest(ActionContext action) throws ConferenceException,
            StorageException, IOException, JDOMException, ParseException {
        
        if(action.getParameters().isEmpty())
            throw new InvalidServletAccessException();
        
        Conference con = action.openConferenceHere();
            
        if(!action.getUser().equals(con.getChairperson()))
            throw new ChairpersonOnlyException(con);
        
        // All papers with checkboxes set to "on" are in the parameters map,
        // these will be set to accepted.
        for(Paper paper : con.getPapers())
            paper.setAccepted(action.getParameters().containsKey(paper.getWikiTitle()));
    }

}
