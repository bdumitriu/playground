/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.converter;

import org.ffplanner.bean.EntityEJB;
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

    private static final Logger logger = Logger.getLogger(EntityConverter.class.getName());

    @Override
    public Object getAsObject(FacesContext context, UIComponent component, String value) {
        if (value == null) {
            return null;
        } else {
            try {
                final InitialContext initialContext = new InitialContext();
                try {
                    final EntityEJB<T> entityEJB = (EntityEJB<T>) initialContext.lookup(getEntityEjbJndiName());
                    try {
                        final Long entityId = Long.valueOf(value);
                        return entityEJB.find(entityId);
                    } catch (NumberFormatException ignored) {
                        return null;
                    }
                } finally {
                    initialContext.close();
                }
            } catch (NamingException e) {
                Logging.getInstance().log(logger, "Failed to find EJB in converter: ", e);
                return null;
            }
        }
    }

    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
        if (getClass().isInstance(value)) {
            return String.valueOf(getId(value));
        } else {
            return null;
        }
    }

    protected abstract Long getId(Object value);

    protected abstract String getEntityEjbJndiName();
}
