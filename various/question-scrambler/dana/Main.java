package dana;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.thoughtworks.xstream.XStream;

/**
 * @author Bogdan Dumitriu
 */
public class Main {

	private static final String PATH_TO_QUESTIONS_FILE =
			"d:\\work\\workrepo\\various\\question-scrambler\\v1.txt";

	private static final boolean HAS_ANSWERS = true;

	private static final String PATH_TO_ANSWERS_FILE =
			"d:\\work\\workrepo\\various\\question-scrambler\\abaca.txt";

	private static final boolean SPACE_BETWEEN_QUESTIONS = true;

/*
	public static void main(String[] args) throws IOException {
		final ArrayList<Question> qs = new ArrayList<Question>();
		qs.add(new Question(1, "Sindromul de condensare pulmonara:\n"
				+ "a) Nu este caracterizat prin prezenta bronhogramei aerice\n"
				+ "b) Apare in pneumoniile interstitiale\n"
				+ "c) Determina cresterea in dimensiuni a teritoriului afectat\n"
				+ "d) Determina retractia scizurilor\n"
				+ "e) Poate aparea in infarctul pulmonar\n"));
		qs.add(new Question(2, "Pneumoniile interstitiale:\n"
				+ "a) Sunt frecvent de cauza bacteriana\n"
				+ "b) Se caracterizeaza prin hili pulmonari mariti, flu conturati\n"
				+ "c) Se caracterizeaza prin prezenta de cordoane opace apicale\n"
				+ "d) Sunt cel mai frecvent produse de Streptococcus pneumoniae.\n"
				+ "e) Prezinta bronhograma aerica\n"));
		final Questions questions = new Questions(qs);

		final Writer writer = new BufferedWriter(new FileWriter("c:/x.txt"));
		final XStream xstream = new XStream();
		xstream.processAnnotations(Questions.class);
		try {
			xstream.toXML(questions, writer);
		} finally {
			writer.close();
		}
	}
*/

	public static void main(String[] args) throws IOException {
		for (int i = 2; i <= 4; i++) {
			shuffle(i);
		}
	}

	private static void shuffle(int shuffleNumber) throws IOException {
		final XStream xstream = new XStream();
		xstream.processAnnotations(Questions.class);
		final File questionsFile = new File(PATH_TO_QUESTIONS_FILE);
		final BufferedReader questionsReader = new BufferedReader(new FileReader(questionsFile));
		
		Questions questions;
		try {
			questions = (Questions) xstream.fromXML(questionsReader);
		} finally {
			questionsReader.close();
		}

		final BufferedReader answersReader =
				HAS_ANSWERS ? new BufferedReader(new FileReader(PATH_TO_ANSWERS_FILE)) : null;
		if (HAS_ANSWERS) {
			for (Question question : questions.questions) {
				String line = answersReader.readLine();
				assert line != null;
				if (line != null) {
					final int dotIndex = line.indexOf('.');
					question.answer = line.charAt(dotIndex + 2);
				}
			}
			answersReader.close();
		}

		Collections.shuffle(questions.questions);

		final Writer writer = new BufferedWriter(new FileWriter(
				new File(questionsFile.getParentFile(), "varianta " + shuffleNumber + ".txt")));
		final Writer answersWriter = HAS_ANSWERS ? new BufferedWriter(new FileWriter(
				new File(questionsFile.getParentFile(), "varianta " + shuffleNumber + " (raspunsuri).txt"))) : null;
		int i = 1;
		for (Question question : questions.questions) {
			writer.write(i + ". ");
			writer.write(question.question);
			if (HAS_ANSWERS) {
				answersWriter.write(i + ". " + question.answer + "\r\n");
			}
			i++;
			if (SPACE_BETWEEN_QUESTIONS) {
				writer.write("\r\n");
			}
		}
		writer.close();
		if (HAS_ANSWERS) {
			answersWriter.close();
		}
	}

	private static void shuffleTest() {
		List<Integer> integers = new ArrayList<Integer>();
		for (int i = 1; i < 51; i++) {
			integers.add(i);
		}
		Collections.shuffle(integers);
		for (Integer anInt : integers) {
			System.out.println(anInt);
		}
	}
}
