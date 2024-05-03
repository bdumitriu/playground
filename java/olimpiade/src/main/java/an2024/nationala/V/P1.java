package an2024.nationala.V;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.util.List;

public class P1 {

    public static void main(String[] args) {
        try {
            final String result = solve(Paths.get("robinhood.in"));
            Files.writeString(Paths.get("robinhood.out"), result, StandardOpenOption.CREATE);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String solve(Path inputFile) throws IOException {
        final List<String> lines = Files.readAllLines(inputFile);
        if (lines.size() < 3) {
            throw new RuntimeException("At least 3 lines expected in input file.");
        }
        final int C;
        try {
            C = Integer.parseInt(lines.get(0));
        } catch (NumberFormatException e) {
            throw new RuntimeException("A natural number expected in the first line of the input file");
        }
        final int n;
        try {
            n = Integer.parseInt(lines.get(1));
        } catch (NumberFormatException e) {
            throw new RuntimeException("A natural number expected in the second line of the input file");
        }
        final String[] timeIntervals = lines.get(2).split(" ");
        if (timeIntervals.length < 2) {
            throw new RuntimeException("At least 2 values expected in the third line of the input file.");
        }
        final int p;
        final int q;
        try {
            p = Integer.parseInt(timeIntervals[0]);
            q = Integer.parseInt(timeIntervals[1]);
        } catch (NumberFormatException e) {
            throw new RuntimeException("Two natural numbers expected in the third line of the input file");
        }

        final int[] targets = new int[n];
        for (int i = 0; i < n; i++) {
            targets[i] = 0;
        }
        int targetsRemaining = n;
        int robinIndex = 0;
        int robinDirection = 1;
        int johnIndex = n - 1;
        int johnDirection = -1;
        int robinRemaining = p;
        int johnRemaining = q;
        int clock = 0;
        do {
            clock++;
            robinIndex += robinDirection;
            if (robinIndex == 0 || robinIndex == n - 1) {
                robinDirection *= -1;
            }
            johnIndex += johnDirection;
            if (johnIndex == 0 || johnIndex == n - 1) {
                johnDirection *= -1;
            }
            robinRemaining--;
            if (robinRemaining == 0) {
                if (targets[robinIndex] == 0) {
                    targetsRemaining--;
                }
                targets[robinIndex]++;
                robinRemaining = p;
            }
            johnRemaining--;
            if (johnRemaining == 0) {
                if (targets[johnIndex] == 0) {
                    targetsRemaining--;
                }
                targets[johnIndex]++;
                johnRemaining = q;
            }
        } while (targetsRemaining > 0);

        final StringBuilder stringBuilder = new StringBuilder();
        switch (C) {
            case 1 -> stringBuilder.append(clock);
            case 2 -> {
                for (int i = 0; i < n; i++) {
                    if (targets[i] == 1) {
                        stringBuilder.append(i + 1).append(" ");
                    }
                }
            }
            case 3 -> {
                int max = 0;
                for (int i = 0; i < n; i++) {
                    if (targets[i] > max) {
                        max = targets[i];
                    }
                }
                stringBuilder.append(max);
                stringBuilder.append(System.lineSeparator());
                for (int i = 0; i < n; i++) {
                    if (targets[i] == max) {
                        stringBuilder.append(i + 1).append(" ");
                    }
                }
            }
            default -> throw new RuntimeException("1 <= C <= 3 not respected!");
        }
        stringBuilder.append(System.lineSeparator());
        return stringBuilder.toString();
    }
}
