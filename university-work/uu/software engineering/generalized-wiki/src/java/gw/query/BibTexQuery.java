/* Generated By:JavaCC: Do not edit this line. BibTexQuery.java */
    package gw.query;

    import java.io.FileInputStream;
    import java.io.StringReader;
    import java.io.Reader;

    public class BibTexQuery implements BibTexQueryConstants {
        public static void main( String[] args )throws ParseException, TokenMgrError {
            //Qparser parser = new Qparser( System.in ) ;
            try{
                BibTexQuery parser = new BibTexQuery( new FileInputStream("test.q")  ) ;
                try{
                    parser.testQuery() ;
                }catch(Exception e)
                {
                    System.out.println("Error occurred while parsing the query");
                    e.printStackTrace();
                }
            }catch(Exception e)
            {
                System.out.println("Error occurred while reading the file");
                e.printStackTrace();
            }
        }
        public static QNode parseQuery( String inString ) {
            Reader reader = new StringReader( inString ) ;
            BibTexQuery parser = new BibTexQuery( reader ) ;
            QNode result = null;
            try{
                    result = parser.parseQuery() ;
            }catch(Exception e)  {
                System.out.println("Parse Error");
                e.printStackTrace();
            }
            return result;
        }

  final public void testQuery() throws ParseException {
 QNode res;
    label_1:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case WORD:
      case OPEN_PAR:
        ;
        break;
      default:
        jj_la1[0] = jj_gen;
        break label_1;
      }
      res = parseQuery();
        System.out.println(res);
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case EOL:
        jj_consume_token(EOL);
        break;
      case 0:
        jj_consume_token(0);
                  System.exit(0);
        break;
      default:
        jj_la1[1] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
  }

  final public QNode parseQuery() throws ParseException {
 QNode root;
    root = expression();
     {if (true) return root;}
    throw new Error("Missing return statement in function");
  }

  final public QNode expression() throws ParseException {
 QNode node = null ; QNode left; QNode right;
    left = term();
    label_2:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case OR_OP:
        ;
        break;
      default:
        jj_la1[2] = jj_gen;
        break label_2;
      }
      jj_consume_token(OR_OP);
      right = term();
             node = new QNode(QNode.OR);
             node.setLeftQNode(left);
             node.setRightQNode(right);
             left = node;
    }
       {if (true) return left;}
    throw new Error("Missing return statement in function");
  }

  final public QNode term() throws ParseException {
 QNode node = null; QNode left; QNode right;
    left = factor();
    label_3:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case AND_OP:
        ;
        break;
      default:
        jj_la1[3] = jj_gen;
        break label_3;
      }
      jj_consume_token(AND_OP);
      right = factor();
           node = new QNode(QNode.AND);
           node.setLeftQNode(left);
           node.setRightQNode(right);
           left = node;
    }
     {if (true) return left;}
    throw new Error("Missing return statement in function");
  }

  final public QNode factor() throws ParseException {
 QNode node; QLeaf leaf;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case WORD:
      leaf = conditon();
     node = new QNode(QNode.SINGLE);
     node.setLeaf(leaf);
     {if (true) return node;}
      break;
    case OPEN_PAR:
      jj_consume_token(OPEN_PAR);
      node = expression();
      jj_consume_token(CLOSE_PAR);
     {if (true) return node;}
      break;
    default:
      jj_la1[4] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    throw new Error("Missing return statement in function");
  }

  final public QLeaf conditon() throws ParseException {
 QLeaf leaf; Token type;Token operator;Token value;String strValue="";
    type = jj_consume_token(WORD);
    operator = jj_consume_token(OPERATOR);
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case WORD:
      value = jj_consume_token(WORD);
         strValue+= value.image;
      break;
    case OPEN_DEL:
      jj_consume_token(OPEN_DEL);
      value = jj_consume_token(WORD);
                 strValue+= value.image;
      label_4:
      while (true) {
        switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
        case WORD:
          ;
          break;
        default:
          jj_la1[5] = jj_gen;
          break label_4;
        }
        value = jj_consume_token(WORD);
                                 strValue+=" " + value.image;
      }
      jj_consume_token(CLOSE_DEL);
      break;
    default:
      jj_la1[6] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
     leaf = new QLeaf(type.image, getOperator(operator.image), strValue);
     {if (true) return leaf;}
    throw new Error("Missing return statement in function");
  }

  final public int getOperator(String op) throws ParseException {
        int operator = 0;
        if(op.equals("is")) operator = 0;
        if(op.equals("not")) operator = 1;
        if(op.equals("lt" )) operator = 2;
        if(op.equals("lte")) operator = 3;
        if(op.equals("gt" )) operator = 4;
        if(op.equals("gte")) operator = 5;

        {if (true) return operator;}
    throw new Error("Missing return statement in function");
  }

  public BibTexQueryTokenManager token_source;
  SimpleCharStream jj_input_stream;
  public Token token, jj_nt;
  private int jj_ntk;
  private int jj_gen;
  final private int[] jj_la1 = new int[7];
  static private int[] jj_la1_0;
  static {
      jj_la1_0();
   }
   private static void jj_la1_0() {
      jj_la1_0 = new int[] {0x30,0x201,0x4,0x2,0x30,0x10,0x90,};
   }

  public BibTexQuery(java.io.InputStream stream) {
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new BibTexQueryTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 7; i++) jj_la1[i] = -1;
  }

  public void ReInit(java.io.InputStream stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 7; i++) jj_la1[i] = -1;
  }

  public BibTexQuery(java.io.Reader stream) {
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new BibTexQueryTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 7; i++) jj_la1[i] = -1;
  }

  public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 7; i++) jj_la1[i] = -1;
  }

  public BibTexQuery(BibTexQueryTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 7; i++) jj_la1[i] = -1;
  }

  public void ReInit(BibTexQueryTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 7; i++) jj_la1[i] = -1;
  }

  final private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken;
    if ((oldToken = token).next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    if (token.kind == kind) {
      jj_gen++;
      return token;
    }
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }

  final public Token getNextToken() {
    if (token.next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    jj_gen++;
    return token;
  }

  final public Token getToken(int index) {
    Token t = token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  final private int jj_ntk() {
    if ((jj_nt=token.next) == null)
      return (jj_ntk = (token.next=token_source.getNextToken()).kind);
    else
      return (jj_ntk = jj_nt.kind);
  }

  private java.util.Vector jj_expentries = new java.util.Vector();
  private int[] jj_expentry;
  private int jj_kind = -1;

  public ParseException generateParseException() {
    jj_expentries.removeAllElements();
    boolean[] la1tokens = new boolean[12];
    for (int i = 0; i < 12; i++) {
      la1tokens[i] = false;
    }
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 7; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 12; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.addElement(jj_expentry);
      }
    }
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = (int[])jj_expentries.elementAt(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  final public void enable_tracing() {
  }

  final public void disable_tracing() {
  }

    }
