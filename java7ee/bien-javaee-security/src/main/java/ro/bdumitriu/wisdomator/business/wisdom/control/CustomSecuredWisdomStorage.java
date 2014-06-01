package ro.bdumitriu.wisdomator.business.wisdom.control;

import ro.bdumitriu.wisdomator.business.security.boundary.AllowedTo;
import ro.bdumitriu.wisdomator.business.security.boundary.Guard;
import ro.bdumitriu.wisdomator.business.security.entity.Permission;

import javax.annotation.PostConstruct;
import javax.ejb.Singleton;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import java.security.Principal;

/**
 * @author Bogdan Dumitriu
 */
@Singleton
@Interceptors(Guard.class)
public class CustomSecuredWisdomStorage {

    private String wisdom;

    @Inject
    Principal principal;

    @PostConstruct
    public void initialize() {
        this.wisdom = "The Java Programming Language Rocks!!!";
    }

    @AllowedTo(Permission.READ)
    public String wisdom() {
        return wisdom;
    }

    @AllowedTo(Permission.WRITE)
    public void wisdom(String wisdom) {
        this.wisdom = wisdom;
    }
}
