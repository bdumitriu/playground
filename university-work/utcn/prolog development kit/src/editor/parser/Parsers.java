package editor.parser;

import editor.PDKToken;
import editor.PDKRootElement;

import javax.swing.event.DocumentEvent;
import javax.swing.text.Element;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import java.io.ByteArrayInputStream;

/**
 * A sort of Toolkit class, providing methods for obtaining a Parser implementations.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 * 
 * Date: Feb 26, 2003
 */
public class Parsers
{
	/**
	 * Returns a default parser.
	 *
	 * @return a default parser.
	 */
	public static Parser getDefaultParser()
	{
		return DefaultParser.getDefaultParser();
	}
}

/**
 * Singleton class which provides a default parser.
 */
class DefaultParser implements Parser
{
	private static DefaultParser dp = null;

	public static DefaultParser getDefaultParser()
	{
		if (dp == null)
		{
			dp = new DefaultParser();
		}
		return dp;
	}

	private DefaultParser()
	{}

	public PDKChange update(DocumentEvent event, Element rootElement)
	{
		PDKRootElement root = (PDKRootElement) rootElement;

		/* get the index of the first element influenced by the change */
		int startIndex = root.getElementIndex(event.getOffset());
		if (startIndex < 0)
			startIndex = 0;

		/* get the first token influenced by the change */
		PDKToken startToken = null;
		try
		{
			startToken = (PDKToken)
				root.getElement(startIndex);
		}
		catch (IndexOutOfBoundsException e)
		{
			/* this might happen when startIndex is forced to 0 above */
			/* if it happends, firstInvalidToken will simply stay null */
		}

		/* get the absolute document offset where to reparse from */
		int startOffset = 0;
		if (startToken != null)
		{
			// this check has to be made for situations like this:
			// |\n|a|b|c|, when token <abc> has start offset of 1
			// and event.getOffset() = 0, i.e. something has been
			// inserted before '\n'. If we simply assigned
			// startToken.getStartOffset() to startOffset, parsing
			// would *eroneously* start from 1 (failing to parse
			// character at index 0).
			if (event.getOffset() >= startToken.getStartOffset())
			{
				startOffset = startToken.getStartOffset();
			}
			else
			{
				// get the start offset of the previous element, if
				// there is one
				startIndex--;
				if (startIndex < 0)
				{
					startIndex = 0;
					startOffset = 0;
				}
				else
				{
					startToken = (PDKToken) root.getElement(startIndex);
					startOffset = startToken.getStartOffset();
				}
			}
		}

		/* create a byte array input stream based on the document content from startOffset onwards */
		Document doc = event.getDocument();
		String docContent = null;
		try
		{
			docContent = doc.getText(startOffset, doc.getLength() - startOffset);
		}
		catch (BadLocationException e)
		{
			e.printStackTrace();
		}

		if (docContent.length() > 0)
		{
			TokenParser parser = new TokenParser(new ByteArrayInputStream(docContent.getBytes()));

			/* delete all invalid tokens from root */
			root.setSize(startIndex);

			/* parse */
			try
			{
				parser.tokenList(root, startOffset);
			}
			catch (ParseException e)
			{
				e.printStackTrace();
			}
		}

		return new PDKChange(0, 0);
	}
}