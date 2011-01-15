package dana;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamConverter;
import com.thoughtworks.xstream.annotations.XStreamOmitField;

/**
 * @author Bogdan Dumitriu
 */
@XStreamConverter(QuestionConverter.class)
public class Question {

	@XStreamAsAttribute
	@XStreamAlias("nr")
	public int nr;

	public String question;

	@XStreamOmitField
	public char answer;

	public Question(int nr, String question) {
		this.nr = nr;
		this.question = question;
	}
}
