package ro.bdumitriu.wisdomator.business.security.control;

import ro.bdumitriu.wisdomator.business.security.entity.Permission;

import javax.annotation.PostConstruct;
import javax.ejb.ConcurrencyManagement;
import javax.ejb.ConcurrencyManagementType;
import javax.ejb.Singleton;
import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
@Singleton
@ConcurrencyManagement(ConcurrencyManagementType.BEAN)
public class InMemoryPermissionRealm {

    private Map<String, EnumSet<Permission>> customStore;

    @PostConstruct
    public void populateRealm() {
        this.customStore = new HashMap<>();
        this.customStore.put("james", EnumSet.allOf(Permission.class));
        this.customStore.put("blogger", EnumSet.of(Permission.READ));
    }

    public EnumSet<Permission> getPermissionForPrincipal(String userName) {
        return this.customStore.getOrDefault(userName, EnumSet.noneOf(Permission.class));
    }
}
