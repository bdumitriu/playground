package org.ffplanner.util;

import org.junit.Assert;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.core.AnyOf.anyOf;
import static org.junit.Assert.assertEquals;

/**
 * @author Bogdan Dumitriu
 */
public class JsfViewHelperTest {

    @Test
    public void testRootConstructor() {
        final String viewString = new JsfViewHelper(JsfViewIds.LOGIN).toString();
        assertEquals("/Login.xhtml", viewString);
    }

    @Test
    public void testRootPath() {
        final String viewString = new JsfViewHelper(JsfViewHelper.Path.ROOT, JsfViewIds.LOGIN).toString();
        assertEquals("/Login.xhtml", viewString);
    }

    @Test
    public void testIncludedPath() {
        final String viewString = new JsfViewHelper(JsfViewHelper.Path.INCLUDED, JsfViewIds.LOGIN).toString();
        assertEquals("Login.xhtml", viewString);
    }

    @Test
    public void testNoModifiers() {
        final String viewString = new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.PROGRAM).toString();
        assertEquals("/auth/Program.xhtml", viewString);
    }

    @Test
    public void testFacesRedirect() {
        final String viewString =
                new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.MY_SCHEDULE, JsfViewHelper.FR).toString();
        assertEquals("/auth/MySchedule.xhtml?faces-redirect=true", viewString);
    }

    @Test
    public void testIncludeViewParams() {
        final String viewString =
                new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.MY_SCHEDULE, JsfViewHelper.IVP).toString();
        assertEquals("/auth/MySchedule.xhtml?includeViewParams=true", viewString);
    }

    @Test
    public void testTwoModifiers() {
        final String viewString =
                new JsfViewHelper(JsfViewHelper.Path.AUTH, JsfViewIds.MY_SCHEDULE, JsfViewHelper.FR_IVP).toString();
        Assert.assertThat(viewString, anyOf(
                equalTo("/auth/MySchedule.xhtml?faces-redirect=true&includeViewParams=true"),
                equalTo("/auth/MySchedule.xhtml?includeViewParams=true&faces-redirect=true")));
    }
}
