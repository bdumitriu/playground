package util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Utils {

    public static String getStringFromResource(String resourceName) throws IOException {
        try (InputStream inputStream = Utils.class.getClassLoader().getResourceAsStream(resourceName)) {
            if (inputStream == null) {
                throw new IOException();
            } else {
                return new String(inputStream.readAllBytes(), StandardCharsets.UTF_8.name());
            }
        }
    }

    public static String getStringFromFile(String fileName) throws IOException {
        return Files.readString(Paths.get(fileName));
    }

    public static void writeStringToFile(String fileName, String contents) throws IOException {
        Files.writeString(Paths.get(fileName), contents);
    }

    public static String getFileNameWithNewExtension(String fileName, String newExtensionWithDot) {
        final int indexOfLastDot = fileName.lastIndexOf('.');
        final String nameWithoutExtension = fileName.substring(0, indexOfLastDot);
        return nameWithoutExtension + newExtensionWithDot;
    }
}
