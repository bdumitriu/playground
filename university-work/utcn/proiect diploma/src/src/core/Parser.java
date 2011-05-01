package core;

import data.DocumentTreeNode;
import enum.TreeLevel;

/**
 * This class provides support for parsing text documents.
 */
public class Parser
{
	/** word separators */
	public static final String wordSeparatos = " ,;:";

	/** sentence separators */
	public static final String sentenceSeparators = ".!?";

	/** paragraph separators */
	public static final String paragraphSeparators = "\n\r";

	/** all separators */
	public static final String separators = wordSeparatos + sentenceSeparators + paragraphSeparators;

	/**
	 * Returns the default separator for the indicated document <code>level</code>.
	 */
	public static char getDefaultSeparator(int level)
	{
		switch (level)
		{
			case TreeLevel.document:
				return '\n';
			case TreeLevel.paragraph:
				return '.';
			case TreeLevel.sentence:
				return ' ';
			default:
				return ' ';
		}
	}

	/**
	 * Parses an entire document and returns the root node of the generated tree representation.
	 */
	public static DocumentTreeNode parseDocument(String text)
	{
		if (text == null)
		{
			return null;
		}

		if (text.length() == 0)
		{
			return null;
		}

		DocumentTreeNode root = new DocumentTreeNode();
		root.setLevel(TreeLevel.document);

		while (!text.equals(""))
		{
			DocumentTreeNode child = parseParagraph(text);

			if (child == null)
			{
				break;
			}

			text = text.substring(child.getLength());

			root.addChildAt(root.getNrChildren(), child);
		}

		if (root.getNrChildren() == 0)
		{
			return null;
		}
		else
		{
			return root;
		}
	}

	/**
	 * Parses an entire paragraph and returns the root node of the generated tree representation.
	 */
	public static DocumentTreeNode parseParagraph(String text)
	{
		if (text == null)
		{
			return null;
		}

		if (text.length() == 0)
		{
			return null;
		}

		DocumentTreeNode root = new DocumentTreeNode();
		root.setLevel(TreeLevel.paragraph);

		char c = text.charAt(0);
		if (paragraphSeparators.indexOf(c) != -1)
		{
			root.setContent("" + c);
			root.setSeparator(true);
			return root;
		}

		while (!text.equals(""))
		{
			DocumentTreeNode child = parseSentence(text);

			if (child == null)
			{
				break;
			}

			text = text.substring(child.getLength());

			root.addChildAt(root.getNrChildren(), child);
		}
		if (root.getNrChildren() == 0)
		{
			return null;
		}
		else
		{
			return root;
		}
	}

	/**
	 * Parses an entire sentence and returns the root node of the generated tree representation.
	 */
	public static DocumentTreeNode parseSentence(String text)
	{
		if (text == null)
		{
			return null;
		}

		if (text.length() == 0)
		{
			return null;
		}

		DocumentTreeNode root = new DocumentTreeNode();
		root.setLevel(TreeLevel.sentence);

		char c = text.charAt(0);
		if (sentenceSeparators.indexOf(c) != -1)
		{
			root.setContent("" + c);
			root.setSeparator(true);
			return root;
		}

		while (!text.equals(""))
		{
			DocumentTreeNode child = parseWord(text);

			if (child == null)
			{
				break;
			}

			text = text.substring(child.getLength());

			root.addChildAt(root.getNrChildren(), child);
		}

		if (root.getNrChildren() == 0)
		{
			return null;
		}
		else
		{
			return root;
		}
	}

	/**
	 * Parses an entire word and returns the root node of the generated tree representation.
	 */
	public static DocumentTreeNode parseWord(String text)
	{
		if (text == null)
		{
			return null;
		}

		if (text.length() == 0)
		{
			return null;
		}

		DocumentTreeNode root = new DocumentTreeNode();
		root.setLevel(TreeLevel.word);

		String word = getNextWord(text);

		if (wordSeparatos.indexOf(text.charAt(0)) != -1)
		{
			root.setSeparator(true);
		}

		if (word == null)
		{
			return null;
		}
		else
		{
			root.setContent(word);
			return root;
		}
	}

	/**
	 * Returns the next word which can be found in the given text.
	 */
	private static String getNextWord(String text)
	{
		if (text == null)
		{
			return null;
		}

		if (text.equals(""))
		{
			return null;
		}

		if (wordSeparatos.indexOf(text.charAt(0)) != -1)
		{
			return "" + text.charAt(0);
		}

		if (separators.indexOf(text.charAt(0)) != -1)
		{
			return null;
		}

		String word = "" + text.charAt(0);
		for (int i = 1; i < text.length(); i++)
		{
			char c = text.charAt(i);
			if (separators.indexOf(c) != -1)
			{
				return word;
			}
			word += c;
		}

		return word;
	}
}