package ro.bdumitriu.wisdomator.business.wisdom.control;

import javax.annotation.PostConstruct;
import javax.annotation.security.DeclareRoles;
import javax.annotation.security.PermitAll;
import javax.annotation.security.RolesAllowed;
import javax.ejb.Singleton;
import javax.inject.Inject;
import java.security.Principal;

/**
 * @author Bogdan Dumitriu
 */
@Singleton
@DeclareRoles("dukes")
public class WisdomStorage {

    private String wisdom;

    @Inject
    Principal principal;

    @PostConstruct
    public void initialize() {
        this.wisdom = "The Java Programming Language Rocks!!!";
    }

    @PermitAll
    public String wisdom() {
        return wisdom;
    }

    @RolesAllowed("dukes")
    public void wisdom(String wisdom) {
        this.wisdom = wisdom;
    }
}
