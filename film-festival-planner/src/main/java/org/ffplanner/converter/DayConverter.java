package org.ffplanner.converter;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.FacesConverter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @author Bogdan Dumitriu
 */
@FacesConverter(value = "dayConverter")
public class DayConverter implements Converter {

    private static final String DAY_FORMAT = "dd/MM/yyyy"; //NON-NLS

    @Override
    public Object getAsObject(FacesContext context, UIComponent component, String value) {
        if (value == null) {
            return null;
        } else {
            try {
                @SuppressWarnings("SimpleDateFormatWithoutLocale")
                final SimpleDateFormat dateFormat = new SimpleDateFormat(DAY_FORMAT);
                return dateFormat.parse(value);
            } catch (ParseException ignored) {
                return null;
            }
        }
    }

    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
        if (value instanceof Date) {
            @SuppressWarnings("SimpleDateFormatWithoutLocale")
            final SimpleDateFormat dateFormat = new SimpleDateFormat(DAY_FORMAT);
            return dateFormat.format((Date) value);
        } else {
            return null;
        }
    }
}
