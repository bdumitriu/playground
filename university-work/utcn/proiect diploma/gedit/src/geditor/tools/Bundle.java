package geditor.tools;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.net.URL;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 24, 2003
 * Time: 12:17:43 PM
 */
public class Bundle {
    protected  static ResourceBundle bundle;

    public static String getString(String key) {
        String value = null;
        try {
            bundle = ResourceBundle.getBundle("geditor.resources.general");
            value = bundle.getString(key);
        } catch (MissingResourceException e) {
            System.out.println("java.util.MissingResourceException: Couldn't find value for: " + key);
        }
        if(value == null) {
            value = "Could not find resource: " + key + "  ";
        }
        return value;
        }

    public static int getInteger(String key) {
        int value = -1;
        try {
            bundle = ResourceBundle.getBundle("geditor.resources.general");
            value = Integer.parseInt(bundle.getString(key));
        } catch (MissingResourceException e) {
            System.out.println("java.util.MissingResourceException: Couldn't find value for: " + key);
        }
        return value;
        }
}
