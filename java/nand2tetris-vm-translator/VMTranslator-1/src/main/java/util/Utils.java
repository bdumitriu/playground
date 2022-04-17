package util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class Utils {

    public static String getStringFromResource(String resourceName) throws IOException {
        try (InputStream inputStream = Utils.class.getClassLoader().getResourceAsStream(resourceName)) {
            if (inputStream == null) {
                return resourceName + " not found";
            } else {
                return new String(inputStream.readAllBytes(), StandardCharsets.UTF_8.name());
            }
        }
    }
}
