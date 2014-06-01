package ro.bdumitriu.wisdomator.business.wisdom.control;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.annotation.security.DeclareRoles;
import javax.ejb.SessionContext;
import javax.ejb.Singleton;
import javax.inject.Inject;
import java.security.Principal;

/**
 * @author Bogdan Dumitriu
 */
@Singleton
@DeclareRoles("dukes")
public class ProgramaticallySecuredWisdomStorage {

    private String wisdom;

    @Inject
    Principal principal;

    @Resource
    SessionContext sessionContext;

    @PostConstruct
    public void initialize() {
        this.wisdom = "The Java Programming Language Rocks!!!";
    }

    public String wisdom() {
        return wisdom;
    }

    public void wisdom(String wisdom) {
        if (!sessionContext.isCallerInRole("dukes")) {
            throw new IllegalStateException("Only dukes can change the wisdom! Are you a blogger?");
        }
        this.wisdom = wisdom;
    }
}
