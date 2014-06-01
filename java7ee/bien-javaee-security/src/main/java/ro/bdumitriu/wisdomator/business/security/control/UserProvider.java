package ro.bdumitriu.wisdomator.business.security.control;

import ro.bdumitriu.wisdomator.business.security.entity.User;

import javax.enterprise.inject.Produces;
import javax.inject.Inject;
import java.security.Principal;

/**
 * @author Bogdan Dumitriu
 */
public class UserProvider {

    @Inject
    Principal principal;

    @Inject
    InMemoryPermissionRealm realm;

    @Produces
    public User fetch() {
        final User user = new User(principal.getName());
        user.setPermissions(realm.getPermissionForPrincipal(principal.getName()));
        return user;
    }
}
