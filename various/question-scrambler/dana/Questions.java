package dana;

import java.util.List;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamImplicit;
import com.thoughtworks.xstream.annotations.XStreamInclude;

/**
 * @author Bogdan Dumitriu
 */
@XStreamInclude(Question.class)
@XStreamAlias("qs")
public class Questions {

	@XStreamImplicit(itemFieldName="q")
	public List<Question> questions;

	public Questions(List<Question> questions) {
		this.questions = questions;
	}
}
