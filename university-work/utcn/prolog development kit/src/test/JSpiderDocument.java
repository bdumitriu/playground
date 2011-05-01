package test;

import editor.*;
import editor.parser.*;

import javax.swing.text.*;
import javax.swing.event.DocumentEvent;
import java.awt.*;

/**
 * Custom StyledDocument implementation.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 *
 * Date: Feb 26, 2003
 */
public class JSpiderDocument extends DefaultStyledDocument
{
	/* style for normal text */
	private Style normalStyle;

	/* style for comments */
	private Style commentStyle;

	/* style for operators */
	private Style operatorStyle;

	/* style for variables */
	private Style variableStyle;

	/* style for functors */
	private Style functorStyle;

	/* style for strings */
	private Style stringStyle;

	private StyleContext sc;
	private Style docStyle;
	private DefaultHighlighter highlighter;
	private Parser parser;

	public PDKRootElement getRoot()
	{
		return root;
	}

	private PDKRootElement root;

	/* make sure no parsing is done unless necessary */
	/* see usage in fireXXXUpdate, insertString, remove and setCharacterAttributes methods */
	private boolean needUpdate = true;

	public JSpiderDocument()
	{
		super();

		// obtain parser
		parser = Parsers.getDefaultParser();

		// create root element
		root = new PDKRootElement(this);

		initializeStyle();

		// initialize document-wide style
		docStyle = getLogicalStyle(0);
		initStyle(docStyle);
		setParagraphAttributes(0, 0, docStyle, true);
	}

	/**
	 * Initialize all document styles.
	 */
	private void initializeStyle()
	{
		sc = (StyleContext) getAttributeContext();

		normalStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(normalStyle, Color.BLACK);

		commentStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(commentStyle, new Color(128, 128, 128));
		StyleConstants.setItalic(commentStyle, true);

		operatorStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(operatorStyle, Color.BLUE);

		variableStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(variableStyle, new Color(102, 14, 122));

		functorStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(functorStyle, Color.RED);

		stringStyle = sc.addStyle(null, null);
		StyleConstants.setForeground(stringStyle, new Color(0, 128, 0));
	}

	public void insertString(int offs, String str, AttributeSet a) throws BadLocationException
	{
		// a remove has been done, we need an update of the structure
		needUpdate = true;

		super.insertString(offs, str, a);

		highlightSyntax(null);
	}

	public void remove(int offs, int len) throws BadLocationException
	{
		// a remove has been done, we need an update of the structure
		needUpdate = true;

		super.remove(offs, len);

		highlightSyntax(null);
	}

	protected void fireInsertUpdate(DocumentEvent e)
	{
		if (parser == null)
		{
			parser = Parsers.getDefaultParser();
		}
		if (root == null)
		{
			root = new PDKRootElement(this);
		}
		if (needUpdate)
		{
			parser.update(e, root);
		}
		super.fireInsertUpdate(e);
	}

	protected void fireRemoveUpdate(DocumentEvent e)
	{
		if (parser == null)
		{
			parser = Parsers.getDefaultParser();
		}
		if (root == null)
		{
			root = new PDKRootElement(this);
		}
		if (needUpdate)
		{
			parser.update(e, root);
		}
		super.fireRemoveUpdate(e);
	}

	protected void fireChangedUpdate(DocumentEvent e)
	{
		if (parser == null)
		{
			parser = Parsers.getDefaultParser();
		}
		if (root == null)
		{
			root = new PDKRootElement(this);
		}
		if (needUpdate)
		{
			parser.update(e, root);
		}
		super.fireChangedUpdate(e);
	}

	public void setCharacterAttributes(int offset, int length, AttributeSet s, boolean replace)
	{
		// this is necessary because otherwise, for each change of character
		// attributes, a reparse would be done while with each change, a DocumentEvent
		// is fired, causing the parser's update method to be called over and over
		// again without any use, also causing random crashes of the editor...
		needUpdate = false;
		super.setCharacterAttributes(offset, length, s, replace);
	}

	/*
	 * Do a bit of syntax highlighting. <code>event</code> *should be* used in order not
	 * to rehighlight tokes that haven't changed. Currently, it is ignored.
	 */
	private void highlightSyntax(DocumentEvent event)
	{
		for (int i = 0; i < root.getElementCount(); i++)
		{
			PDKToken tok = (PDKToken) root.getElement(i);
			switch (tok.getType())
			{
				case Constants.SINGLE_LINE_COMMENT_ELEMENT_TYPE:
				case Constants.MULTI_LINE_COMMENT_ELEMENT_TYPE:
				case Constants.UNCLOSED_SINGLE_LINE_COMMENT_ELEMENT_TYPE:
				case Constants.UNCLOSED_MULTI_LINE_COMMENT_ELEMENT_TYPE:
					setCharacterAttributes(tok.getStartOffset(), tok.getEndOffset() + 1,
						commentStyle, true);
					break;
				case Constants.OPERATOR_ELEMENT_TYPE:
					setCharacterAttributes(tok.getStartOffset(), tok.getEndOffset() + 1,
						operatorStyle, true);
					break;
				case Constants.VARIABLE_ELEMENT_TYPE:
					setCharacterAttributes(tok.getStartOffset(), tok.getEndOffset() + 1,
						variableStyle, true);
					break;
				case Constants.WORD_ELEMENT_TYPE:
					setCharacterAttributes(tok.getStartOffset(), tok.getEndOffset() + 1,
						functorStyle, true);
					break;
				case Constants.STRING_ELEMENT_TYPE:
				case Constants.QUOTED_ITEM_ELEMENT_TYPE:
				case Constants.UNCLOSED_STRING_ELEMENT_TYPE:
				case Constants.UNCLOSED_QUOTED_ITEM_ELEMENT_TYPE:
					setCharacterAttributes(tok.getStartOffset(), tok.getEndOffset() + 1,
						stringStyle, true);
					break;
				default:
					setCharacterAttributes(tok.getStartOffset(), tok.getEndOffset() + 1,
						normalStyle, true);
			}
		}
	}

	/* just for testing, currently not used */
	private void highlightCurrentLine()
	{
		try
		{
			highlighter.addHighlight(0, 100, new DefaultHighlighter.DefaultHighlightPainter(new Color(200, 200, 200, 150)));
		}
		catch (BadLocationException e)
		{
			e.printStackTrace();  //To change body of catch statement use Options | File Templates.
		}
	}

	/* just for testing, currently not used */
	private void indent(int offs, String str)
	{
		char[] ins = str.toCharArray();
		try
		{
			if ((ins[0] == '\n') &&
				((getText(offs - 2, 2).equals(":-")) || (getText(offs - 1, 1).equals(","))))
			{
				super.insertString(offs + 1, "\t", null);
			}
		}
		catch (BadLocationException e)
		{
		}
	}

	private void initStyle(Style style)
	{
		StyleConstants.setFontFamily(style, "Monospaced");
		//StyleConstants.setTabSet(style, createTabSet(style));
	}

	/* just for testing, currently not used */
	private TabSet createTabSet(Style style)
	{
		TabStop[] tStops = new TabStop[1000];
		int sizeOfSpace = 7;	/* size of ' ' character in Monospace */
		int tabSize = 8;

		for (int i = 0; i < 1000; i++)
		{
			tStops[i] = new TabStop(i * sizeOfSpace * tabSize);
		}

		return new TabSet(tStops);
	}

	/* just for testing, currently not used */
	public void setHighlighter(DefaultHighlighter highlighter)
	{
		this.highlighter = highlighter;
	}
}
