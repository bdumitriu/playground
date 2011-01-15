package dana;

import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

/**
 * @author Bogdan Dumitriu
 */
public class QuestionConverter implements Converter {

	@Override
	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		final Question question = (Question) source;
		writer.addAttribute("nr", String.valueOf(question.nr));
		writer.setValue(question.question);
	}

	@Override
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		final String nrAsString = reader.getAttribute("nr");
		final int nr = Integer.parseInt(nrAsString);
		final String question = reader.getValue();

		return new Question(nr, question);
	}

	@Override
	public boolean canConvert(Class type) {
		return type.equals(Question.class);
	}
}
