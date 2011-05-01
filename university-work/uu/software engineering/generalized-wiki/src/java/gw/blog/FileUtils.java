package gw.blog;
import java.io.File;

public class FileUtils {
    
    private FileUtils() {
        // No instantiation
    }
    
    public static String dirname(String input) {
        File file = new File(input);
        return file.getParent();
    }
    
    public static String basename(String input) {
        File file = new File(input);
        return file.getName();
    }
    
}
