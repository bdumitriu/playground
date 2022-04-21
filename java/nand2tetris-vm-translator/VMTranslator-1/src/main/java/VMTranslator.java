import util.Utils;

import java.io.IOException;

public class VMTranslator {

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("usage: java " + VMTranslator.class.getName() + " <input-file> [<output-file>]");
        } else {
            final String inputFileName;
            final String outputFileName;
            if (args.length == 1) {
                inputFileName = args[0];
                outputFileName = Utils.getFileNameWithNewExtension(inputFileName, ".asm");
            } else {
                inputFileName = args[0];
                outputFileName = args[1];
            }
            translateProgram(inputFileName, outputFileName);
        }
    }

    private static void translateProgram(String inputFileName, String outputFileName) {
        final String program;
        try {
            program = Utils.getStringFromFile(inputFileName);
        } catch (IOException e) {
            System.out.println("Couldn't read file: " + inputFileName);
            return;
        }
        final VMProgramTranslator translator = new VMProgramTranslator();
        program.lines().forEach(translator::translateAndAppend);
        try {
            Utils.writeStringToFile(outputFileName, translator.getProgram());
        } catch (IOException e) {
            System.out.println("Couldn't write file: " + outputFileName);
        }
    }
}
