package org.ffplanner.util;

import org.apache.commons.lang3.StringUtils;

import javax.faces.context.FacesContext;
import java.io.IOException;
import java.util.EnumSet;

/**
 * A helper to reliably build Strings like "/auth/Program.xhtml?faces-redirect=true&includeViewParams=true" from
 * components.
 *
 * @author Bogdan Dumitriu
 */
public class JsfViewHelper {

    public enum Path {

        INCLUDED(""),

        ROOT("/"),

        AUTH("/auth/");

        private final String path;

        Path(String path) {
            this.path = path;
        }

        @Override
        public String toString() {
            return path;
        }
    }

    public enum Modifier {

        FACES_REDIRECT("faces-redirect=true"),

        INCLUDE_VIEW_PARAM("includeViewParams=true");

        private final String modifier;

        Modifier(String modifier) {
            this.modifier = modifier;
        }

        @Override
        public String toString() {
            return modifier;
        }
    }

    public static final EnumSet<Modifier> FR_IVP = EnumSet.of(Modifier.FACES_REDIRECT, Modifier.INCLUDE_VIEW_PARAM);

    public static final EnumSet<Modifier> FR = EnumSet.of(Modifier.FACES_REDIRECT);

    public static final EnumSet<Modifier> IVP = EnumSet.of(Modifier.INCLUDE_VIEW_PARAM);

    private final Path path;

    private final String viewId;

    private final EnumSet<Modifier> modifiers;

    public JsfViewHelper(String viewId) {
        this(Path.ROOT, viewId);
    }

    public JsfViewHelper(Path path, String viewId) {
        this(path, viewId, EnumSet.noneOf(Modifier.class));
    }

    public JsfViewHelper(Path path, String viewId, EnumSet<Modifier> modifiers) {
        this.path = path;
        this.viewId = viewId;
        this.modifiers = modifiers;
    }

    @Override
    public String toString() {
        final StringBuilder stringBuilder = new StringBuilder(path.toString());
        stringBuilder.append(viewId);
        final String modifiersString = StringUtils.join(modifiers, "&");
        if (StringUtils.isNotEmpty(modifiersString)) {
            stringBuilder.append("?");
            stringBuilder.append(modifiersString);
        }
        return stringBuilder.toString();
    }

    public void redirectTo() {
        redirectTo(null);
    }

    public void redirectTo(String fromAction) {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        facesContext.getApplication().getNavigationHandler().handleNavigation(facesContext, fromAction, toString());
    }

    public void externalRedirectTo() {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        try {
            facesContext.getExternalContext().redirect(FacesUtils.getUrlToView(facesContext, toString()));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
