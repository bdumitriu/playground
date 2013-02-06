package org.ffplanner.converter;

import org.ffplanner.util.Logging;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
public abstract class EntityConverter<T> implements Converter {

    protected static final Logger logger = Logger.getLogger(EntityConverter.class.getName());

    @Override
    public T getAsObject(FacesContext context, UIComponent component, String value) {
        if (value == null) {
            return null;
        } else {
            try {
                final InitialContext initialContext = new InitialContext();
                try {
                    return getAsObject(value, initialContext);
                } finally {
                    initialContext.close();
                }
            } catch (NamingException e) {
                Logging.getInstance().log(logger, "Failed to find bean in converter: ", e);
                return null;
            }
        }
    }

    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
        if (getEntityClass().isInstance(value)) {
            return getAsString(getEntityClass().cast(value));
        } else {
            return null;
        }
    }

    protected abstract Class<T> getEntityClass();

    protected abstract T getAsObject(String value, InitialContext initialContext) throws NamingException;

    public abstract String getAsString(T value);
}
