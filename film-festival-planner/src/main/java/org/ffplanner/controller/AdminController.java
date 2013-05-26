package org.ffplanner.controller;

import org.ffplanner.bean.programme.FestivalProgrammeBean;
import org.ffplanner.entity.User;
import org.ffplanner.entity.UserRole;
import org.ffplanner.qualifier.LoggedInUser;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.Serializable;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
@Named(value = "admin")
@RequestScoped
public class AdminController implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final Logger logger = Logger.getLogger(AdminController.class.getName());

    @Inject @LoggedInUser
    private User user;

    @Inject
    private FestivalProgrammeBean festivalProgrammeBean;

    public void rereadFestivalData() {
        if (user != null && user.hasRole(UserRole.ADMIN)) {
            festivalProgrammeBean.reinitialize();
        }
    }
}
