package org.ffplanner.util;

/**
 * @author Bogdan Dumitriu
 */
@SuppressWarnings("InterfaceNeverImplemented")
public interface JsfViews {

    JsfViewHelper LOGIN_TARGET = new JsfViewHelper(JsfViewIds.LOGIN);

    JsfViewHelper UNAUTHORISED_TARGET = new JsfViewHelper(JsfViewIds.UNAUTHORISED);

    JsfViewHelper PROGRAM_REDIRECT_TARGET =
            new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.PROGRAM, JsfViewHelper.FR_IVP);

    JsfViewHelper PROGRAM_TARGET = new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.PROGRAM);

    JsfViewHelper MY_SCHEDULE_REDIRECT_TARGET =
            new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.MY_SCHEDULE, JsfViewHelper.FR_IVP);

    JsfViewHelper MY_SCHEDULE_TARGET = new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.MY_SCHEDULE);
}
