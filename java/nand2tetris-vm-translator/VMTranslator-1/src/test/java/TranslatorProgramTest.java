import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import util.Utils;

import java.io.IOException;
import java.util.stream.Stream;

public class TranslatorProgramTest {

    static Stream<Arguments> resourceNames() {
        return Stream.of(
                Arguments.of("vm/BasicTest.vm"),
//                Arguments.of("vm/PointerTest.vm"),
                Arguments.of("vm/SimpleAdd.vm"),
                Arguments.of("vm/StackTest.vm"));
//                Arguments.of("vm/StaticTest.vm"));
    }

    @DisplayName("All commands from example files are translated successfully")
    @ParameterizedTest(name = "{index}: {0}")
    @MethodSource("resourceNames")
    public void testAllCommandsTranslate(String resourceName) throws IOException {
        final String program = Utils.getStringFromResource(resourceName);
        final Translator translator = new Translator();
        program.lines().forEach(line -> Assertions.assertDoesNotThrow(() -> translator.translate(line)));
    }
}
