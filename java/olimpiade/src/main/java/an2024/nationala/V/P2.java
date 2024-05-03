package an2024.nationala.V;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.*;

public class P2 {

    public static void main(String[] args) {
        try {
            final String result = solve(Paths.get("puzzle.in"));
            Files.writeString(Paths.get("puzzle.out"), result, StandardOpenOption.CREATE);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String solve(Path inputFile) throws IOException {
        final List<String> lines = Files.readAllLines(inputFile);
        if (lines.size() < 2) {
            throw new RuntimeException("At least 2 lines expected in input file.");
        }
        final String[] firstLineValues = lines.get(0).split(" ");
        if (firstLineValues.length < 3) {
            throw new RuntimeException("At least 3 values expected in the first line of the input file.");
        }
        final int C;
        final int N;
        final int K;
        try {
            C = Integer.parseInt(firstLineValues[0]);
            N = Integer.parseInt(firstLineValues[1]);
            K = Integer.parseInt(firstLineValues[2]);
        } catch (NumberFormatException e) {
            throw new RuntimeException("Three natural numbers expected in the first line of the input file");
        }
        final String[] labels = lines.get(1).split(" ");
        if (labels.length < N) {
            throw new RuntimeException("At least " + N + " values expected in the second line of the input file.");
        }

        final Stack<String> groups = new Stack<>();
        groups.push(labels[0]);
        for (int i = 1; i < N; i++) {
            final String group = groups.pop();
            if (puzzleMatch(group, labels[i])) {
                groups.push(puzzleCombine(group, labels[i]));
            } else {
                groups.push(group);
                groups.push(labels[i]);
            }
        }

        groups.sort((label1, label2) -> {
            int distinctDigits1 = getHowManyDistinctDigits(label1);
            int distinctDigits2 = getHowManyDistinctDigits(label2);
            if (distinctDigits1 < distinctDigits2) {
                return -1;
            } else if (distinctDigits1 > distinctDigits2) {
                return 1;
            } else {
                return -Integer.compare(Integer.parseInt(label1), Integer.parseInt(label2));
            }
        });

        final StringBuilder stringBuilder = new StringBuilder();
        switch (C) {
            case 1 -> stringBuilder.append(groups.size());
            case 2 -> {
                for (int i = 0; i < K-1; i++) {
                    stringBuilder.append(groups.pop()).append(" ");
                }
                stringBuilder.append(groups.pop());
            }
            default -> throw new RuntimeException("1 <= C <= 2 not respected!");
        }
        stringBuilder.append(System.lineSeparator());
        return stringBuilder.toString();
    }

    private static boolean puzzleMatch(String group, String label) {
        final int[] matches = new int[10];
        for (char c : group.toCharArray()) {
            matches[Integer.parseInt(String.valueOf(c))]++;
        }
        int nrMatches = 0;
        for (char c : label.toCharArray()) {
            final int index = Integer.parseInt(String.valueOf(c));
            if (matches[index] > 0) {
                nrMatches++;
                matches[index]--;
            }
        }
        return nrMatches >= 3;
    }

    private static String puzzleCombine(String group, String label) {
        return group.substring(0, Math.min(4, group.length())) + label.substring(Math.max(0, label.length() - 4));
    }

    private static int getHowManyDistinctDigits(String label) {
        final int[] digits = new int[10];
        for (char c : label.toCharArray()) {
            digits[Integer.parseInt(String.valueOf(c))] = 1;
        }
        int distinctDigits = 0;
        for (int digit : digits) {
            if (digit == 1) {
                distinctDigits++;
            }
        }
        return distinctDigits;
    }
}
