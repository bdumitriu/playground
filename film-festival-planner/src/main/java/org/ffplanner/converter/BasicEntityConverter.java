package org.ffplanner.converter;

import org.ffplanner.bean.BasicEntityBean;

import javax.naming.InitialContext;
import javax.naming.NamingException;

/**
 * @author Bogdan Dumitriu
 */
public abstract class BasicEntityConverter<T> extends EntityConverter<T> {

    @Override
    protected T getAsObject(String value, InitialContext initialContext) throws NamingException {
        final BasicEntityBean<T> entityBean = (BasicEntityBean<T>) initialContext.lookup(getEntityEjbJndiName());
        try {
            final Long entityId = Long.valueOf(value);
            return entityBean.find(entityId);
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    @Override
    public String getAsString(T value) {
        return String.valueOf(getId(value));
    }

    protected abstract String getEntityEjbJndiName();

    protected abstract Long getId(T value);
}
