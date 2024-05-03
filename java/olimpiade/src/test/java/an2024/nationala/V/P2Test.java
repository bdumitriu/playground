package an2024.nationala.V;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import util.TestUtils;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

public class P2Test {

    static Stream<Arguments> resourceNames() throws URISyntaxException {
        return TestUtils.getAllFilesInResourceFolderWithExtension(
                "an2024/nationala/V/P2", "in")
                .map(path -> Arguments.of(path, TestUtils.replaceExtension(path, "out")));
    }

    @DisplayName("All exam tests execute successfully")
    @ParameterizedTest(name = "{index}: {0}")
    @MethodSource("resourceNames")
    public void testAllExamFiles(Path inputPath, Path outputPath) throws IOException {
        final String expectedResult = Files.readString(outputPath);
        final String actualResult = P2.solve(inputPath);
        Assertions.assertEquals(expectedResult, actualResult);
    }
}
