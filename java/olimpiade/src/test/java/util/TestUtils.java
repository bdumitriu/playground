package util;

import org.junit.jupiter.params.provider.Arguments;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class TestUtils {

    public static Stream<Path> getAllFilesInResourceFolder(String resourceFolder) {
        final URL testRoot = TestUtils.class.getClassLoader().getResource(resourceFolder);
        assert testRoot != null;
        try {
            return Files.walk(Paths.get(testRoot.toURI()));
        } catch (IOException | URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    public static Stream<Path> getAllFilesInResourceFolderWithExtension(String resourceFolder, String extension) {
        final String canonicalExtension = getCanonicalExtension(extension);
        return getAllFilesInResourceFolder(resourceFolder)
                .filter(path -> path.getFileName().toString().toLowerCase().endsWith(canonicalExtension));
    }

    public static Path replaceExtension(Path path, String replacementExtension) {
        final String canonicalExtension = getCanonicalExtension(replacementExtension);
        final String fileName = path.getFileName().toString();
        final String fileNameWithoutExtension = fileName.substring(0, fileName.lastIndexOf("."));
        return path.resolveSibling(fileNameWithoutExtension + canonicalExtension);
    }

    private static String getCanonicalExtension(String extension) {
        return extension.startsWith(".") ? extension : "." + extension;
    }
}
