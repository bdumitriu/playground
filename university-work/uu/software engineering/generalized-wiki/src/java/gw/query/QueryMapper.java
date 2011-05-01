package gw.query;

public class QueryMapper {
    final public static  String[] stringValues = 
        {"title","author","editor","pages","publisher","institution"};

    public static String getRootQuery(QNode node) {
        if(node.isAndNode()) {
            return "//tuple[ Entry[ (" + getNodeQuery(node.getLeftQNode()) + " and " + getNodeQuery(node.getRightQNode()) + ") ] ]";
        }
        else if(node.isOrNode()) {
            return "//tuple[ Entry[ " + getNodeQuery(node.getLeftQNode()) + " or " + getNodeQuery(node.getRightQNode()) + " ] ]";
        }
        else{
            return  "//tuple[ Entry[ " + getLeafQuery(node) + "] ]";
        }
    }
    
    private static String getNodeQuery(QNode node) {
        if(node.isAndNode()) {
           return getNodeQuery(node.getLeftQNode()) + " and " + getNodeQuery(node.getRightQNode());
        }
        else if(node.isOrNode()) {
            return "("+getNodeQuery(node.getLeftQNode()) + " or " + getNodeQuery(node.getRightQNode())+")";
        }
        else {
            return getLeafQuery(node);
        }
    }
    
    private static String getLeafQuery(QNode node) {
        String type  = node.getLeaf().getType();
        int oper     = node.getLeaf().getOperator();
        String value = node.getLeaf().getValue();
        String query = "";
        String upperCase = "'ABCDEFGHIJKLMNOPQRSTUVWXYZ'";
        String lowerCase = "'abcdefghijklmnopqrstuvwxyz'";
        // 'ABCD...Z'
        if(type.equals("type"))
        {
            query += "translate(string,"+upperCase+","+lowerCase+") = '" + value + "'";
        }
        else
        {
            query += "list/Field[string = '"
                   + type
                   + "']/*[name() = 'QWords' or name() = 'Words']" ;

            if(isAStringValue(type))
            {
                query = "contains(translate("+query+","+ upperCase +"," +lowerCase +"),'"+value+"')";
                if(oper == QLeaf.NOTEQUAL)
                    query = "not("+query+")";
            }
            else
                query += "/list/string "+createOperator(oper) + " " + value + "";
        }
        return query;
    }
    
    private static String createOperator(int op) {
        String oper = "";
        if(op == QLeaf.EQUAL)
            oper = " = ";
        if(op == QLeaf.NOTEQUAL)
            oper = " != ";
        if(op == QLeaf.LT)
            oper = " < ";
        if(op == QLeaf.LTE)
            oper = " <= ";
        if(op == QLeaf.GT)
            oper = " > ";
        if(op == QLeaf.GTE)
            oper = " >= ";
        return oper;
    }
    
    private static boolean isAStringValue(String value){
        for (int i=0; i<stringValues.length; i++) {
            if(value.equals(stringValues[i]))
                return true;
        }
        return false;
    }

    public static void main(String[] args) {
        //String query = "type == article & year < 1999 | author != vermaas & year < 2005";
        String query = "type == article & (year < 1999 | author == vermaas)";
        QNode node = BibTexQuery.parseQuery(query);
        // query in the tree structure
        System.out.println(node);
        System.out.println();
        // xslt query
        System.out.println(QueryMapper.getRootQuery(node));
    }
}