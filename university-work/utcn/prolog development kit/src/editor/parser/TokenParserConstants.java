/* Generated By:JavaCC: Do not edit this line. TokenParserConstants.java */
package editor.parser;

public interface TokenParserConstants {

  int EOF = 0;
  int WHITESPACE = 1;
  int SINGLE_LINE_COMMENT = 2;
  int UNCLOSED_SINGLE_LINE_COMMENT = 3;
  int MULTI_LINE_COMMENT = 4;
  int UNCLOSED_MULTI_LINE_COMMENT = 5;
  int OPERATOR = 6;
  int VARIABLE = 7;
  int NUMBER = 8;
  int WORD = 9;
  int OPEN_ROUND_BRACKET = 10;
  int CLOSED_ROUND_BRACKET = 11;
  int OPEN_SQUARE_BRACKET = 12;
  int CLOSED_SQUARE_BRACKET = 13;
  int OPEN_CURLY_BRACKET = 14;
  int CLOSED_CURLY_BRACKET = 15;
  int DOT = 16;
  int QUOTED_ITEM = 17;
  int UNCLOSED_QUOTED_ITEM = 18;
  int STRING = 19;
  int UNCLOSED_STRING = 20;
  int UNKNOWN_CHAR = 21;
  int ALPHA = 22;
  int DIGIT = 23;

  int DEFAULT = 0;

  String[] tokenImage = {
    "<EOF>",
    "<WHITESPACE>",
    "<SINGLE_LINE_COMMENT>",
    "<UNCLOSED_SINGLE_LINE_COMMENT>",
    "<MULTI_LINE_COMMENT>",
    "<UNCLOSED_MULTI_LINE_COMMENT>",
    "<OPERATOR>",
    "<VARIABLE>",
    "<NUMBER>",
    "<WORD>",
    "\"(\"",
    "\")\"",
    "\"[\"",
    "\"]\"",
    "\"{\"",
    "\"}\"",
    "\".\"",
    "<QUOTED_ITEM>",
    "<UNCLOSED_QUOTED_ITEM>",
    "<STRING>",
    "<UNCLOSED_STRING>",
    "<UNKNOWN_CHAR>",
    "<ALPHA>",
    "<DIGIT>",
  };

}
