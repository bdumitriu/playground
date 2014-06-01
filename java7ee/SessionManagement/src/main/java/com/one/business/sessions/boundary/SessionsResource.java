package com.one.business.sessions.boundary;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.json.Json;
import javax.json.JsonObject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@Path("sessions")
public class SessionsResource {

    @Inject
    SessionProvider sessionProvider;

//    @GET
//    public OneSession messages() {
//        return new OneSession("duke");
//    }

    @GET
    public JsonObject messages() {
        return Json.createObjectBuilder().add("name-json", "duke").build();
    }
}
