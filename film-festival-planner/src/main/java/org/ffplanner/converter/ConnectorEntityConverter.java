package org.ffplanner.converter;

import org.ffplanner.bean.ConnectorEntityBean;

import javax.naming.InitialContext;
import javax.naming.NamingException;

/**
 * @author Bogdan Dumitriu
 */
public abstract class ConnectorEntityConverter<T> extends EntityConverter<T> {

    @Override
    protected T getAsObject(String value, InitialContext initialContext) throws NamingException {
        final ConnectorEntityBean<T> entityBean =
                (ConnectorEntityBean<T>) initialContext.lookup(getEntityEjbJndiName());
        try {
            final String[] ids = value.split("\\|");
            if (ids.length != 2) {
                return null;
            }
            final Long leftId = Long.valueOf(ids[0]);
            final Long rightId = Long.valueOf(ids[1]);
            return entityBean.find(leftId, rightId);
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    @Override
    public String getAsString(T value) {
        return String.valueOf(getLeftId(value) + "|" + getRightId(value));
    }

    protected abstract String getEntityEjbJndiName();

    protected abstract Long getLeftId(T value);

    protected abstract Long getRightId(T value);
}
