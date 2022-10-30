import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

public class CommentTests {

    private List<String> removeNonCommandLines(List<String> lineListFromFile) {
        return lineListFromFile.stream()
                .map(line -> line.replaceFirst("//.*", "").trim())
                .filter(line -> !line.isEmpty())
                .toList();
    }

    private List<String> removeNonCommandLinesV2(List<String> lineListFromFile) {
        return lineListFromFile.stream()
                .map(line -> line.replaceFirst("//.*", ""))
                .map(String::trim)
                .filter(Predicate.not(String::isEmpty))
                .toList();
    }

    private List<String> removeNonCommandLinesV3(List<String> lineListFromFile) {
        final List<String> result = new LinkedList<String>();
        for (String line : lineListFromFile) {
            final String lineWithoutTrailingComments = line.replaceFirst("//.*", "");
            final String lineWithoutLeadingOrTrailingWhitespace = lineWithoutTrailingComments.trim();
            if (!lineWithoutLeadingOrTrailingWhitespace.isEmpty()) {
                result.add(lineWithoutLeadingOrTrailingWhitespace);
            }
        }
        return result;
    }

    @Test
    public void testComments() {
        final String originalCode = """
                // whole line should disappear
                should not disappear
                should not disappear// only comment should disappear
                should not disappear // only comment and preceding space should disappear
                should not disappear \t\t // only comment and preceding whitespace should disappear
                should not disappear // but // all // of // this // should
                should not disappear except void comment that follows//
                 // whole line should disappear even if it starts with a space
                \t   \t\t \t// whole line should disappear even if it starts with whitespace
                // this and next empty lines should all disappear
                   \n
                \t \t   \n
                \n
                \r
                \r\n
                foo bar baz""";
        final String codeWithoutCommentsAndNewLines = """
                should not disappear
                should not disappear
                should not disappear
                should not disappear
                should not disappear
                should not disappear except void comment that follows
                foo bar baz""";
        Assertions.assertEquals(codeWithoutCommentsAndNewLines,
                toFileContent(removeNonCommandLinesV2(toLineList(originalCode))));
    }


    private List<String> toLineList(String fileContent) {
        return List.of(fileContent.split("\\R"));
    }

    private String toFileContent(List<String> lines) {
        return String.join(System.lineSeparator(), lines);
    }
}
