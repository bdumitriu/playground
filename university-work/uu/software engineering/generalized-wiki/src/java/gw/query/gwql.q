
options {
STATIC = false ;
LOOKAHEAD=1;
}
PARSER_BEGIN(BibTexQuery)
    package gw.query;

    import java.io.FileInputStream;
    import java.io.StringReader;
    import java.io.Reader;

    public class BibTexQuery {
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
    }
PARSER_END(BibTexQuery)


TOKEN : { < AND_OP : ["a","A"]["n","N"]["d","D"] > }
TOKEN : { < OR_OP  : ["o","O"]["r","R"]  > }
TOKEN : { < OPERATOR : "lt" | "is" | "gt" | "lte" | "not" | "gte" > }
TOKEN : { < WORD : (["A"-"Z","a"-"z","0"-"9","_"])+ > }
TOKEN : { < OPEN_PAR : "(" > }
TOKEN : { < CLOSE_PAR : ")" > }
TOKEN : { < OPEN_DEL : "{" > }
TOKEN : { < CLOSE_DEL : "}" > }
TOKEN : { <EOL: "\n" | "\r" | "\r\n" > }
SKIP : {  " " | "\t" }

void testQuery() : 
{QNode res;}
{
    (
       res = parseQuery()
       {System.out.println(res); }
       (
          <EOL>
          |<EOF> {System.exit(0);}
        )
    )*
}

QNode parseQuery() : 
{QNode root;}
{
    root = expression()
    {return root;}
}


QNode expression() :
{QNode node = null ; QNode left; QNode right;}
{
      left  = term()
      (
         <OR_OP> right = term()
         {
             node = new QNode(QNode.OR);
             node.setLeftQNode(left);
             node.setRightQNode(right);
             left = node;
         }
      )*
      {return left;}
      //<EOL>
}

QNode term() :
{QNode node = null; QNode left; QNode right;}
{
    left = factor()
    (
       <AND_OP>  right = factor()
       {
           node = new QNode(QNode.AND);
           node.setLeftQNode(left);
           node.setRightQNode(right);
           left = node;
       }
    )*
    {return left;}
}

QNode factor() :
{QNode node; QLeaf leaf;}
{
    leaf = conditon()
    {node = new QNode(QNode.SINGLE);}
    {node.setLeaf(leaf);}
    {return node; }
    |
    <OPEN_PAR> node = expression() <CLOSE_PAR>
    {return node;}
}

QLeaf conditon() :
{QLeaf leaf; Token type;Token operator;Token value;String strValue="";}
{
    type = <WORD> 
    operator = <OPERATOR>  
    (
        value = <WORD> 
        {strValue+= value.image;}
        | 
        (
                <OPEN_DEL> 
                value = <WORD>
                {strValue+= value.image;}
                (value = <WORD> {strValue+=" " + value.image;} )* 
                <CLOSE_DEL>
         )
    )
    {leaf = new QLeaf(type.image, getOperator(operator.image), strValue);}
    {return leaf;}
}

int getOperator(String op) : {}
{
    {
        int operator = 0;
        if(op.equals("is")) operator = 0;
        if(op.equals("not")) operator = 1;
        if(op.equals("lt" )) operator = 2;
        if(op.equals("lte")) operator = 3;
        if(op.equals("gt" )) operator = 4;
        if(op.equals("gte")) operator = 5;

        return operator;
    }
}

