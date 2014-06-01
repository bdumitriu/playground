package ro.bdumitriu.wisdomator.business.wisdom.boundary;

import ro.bdumitriu.wisdomator.business.wisdom.control.CustomSecuredWisdomStorage;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@Path("wisdom")
@Produces(MediaType.TEXT_PLAIN)
public class WisdomResource {

    @Inject
    CustomSecuredWisdomStorage wisdomStorage;

    @GET
    public String wisdom() {
        return wisdomStorage.wisdom();
    }

    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.TEXT_HTML)
    public String wisdom(@FormParam("wisdom") String wisdom) {
        wisdomStorage.wisdom(wisdom);
        return "thanks!";
    }
}
