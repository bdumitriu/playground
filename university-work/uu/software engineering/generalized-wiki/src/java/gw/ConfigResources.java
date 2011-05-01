package gw;

import java.util.ListResourceBundle;
import java.text.SimpleDateFormat;

/**
 * Container class for all configuration resources.
 * @author Patrick Camphuijsen
 * @author Jeroen Zuijderwijk
 */   
public class ConfigResources extends ListResourceBundle {

   public Object[][] getContents() {
      return contents;
   }

/**
 * Contains all configuration resources, like date format, largest size for uploadable files, etc. for the wiki system
 */
   static final Object[][] contents = {
      {"Date.Format", new SimpleDateFormat("dd-MM-yyyy z HH:mm:ss")},
      {"AttachFile.MaxUpload", new Integer(20)},
      {"AttachFile.MaxFileSize", new Long(1048576)}
   }; 
}
