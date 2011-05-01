package gw.query;

import gw.*;
import java.util.StringTokenizer;
import java.util.Date;

// Servlets
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

// Lucene
import org.apache.lucene.index.*;
import org.apache.lucene.document.*;

/**
 * Generates content in GraphViz .DOT format that describes a graph with the 
 * documents in the wiki as nodes and the hyperlinks in those files as edges.
 * It gets this information from the index.
 **/
public class StatisticsGraph extends GwServlet {

    /**
     * Forwards to goGet.
     **/
    public void doPost( HttpServletRequest request, HttpServletResponse response ) throws ServletException, IOException
    {
        doGet(request, response);
    }

    /**
     * Generates the DOT code.
     **/
    public void doGet( HttpServletRequest request, HttpServletResponse response ) throws ServletException, IOException 
    {

        PrintWriter cout = response.getWriter();
        cout.println( usageInfo() );
        cout.println( "digraph G {" );
        cout.println( legendBox() );

        // the actual graph
        IndexReader r = IndexReader.open( Search.indexPath );
        int num = r.numDocs();
        // for all Documents in the index:
        for ( int i = 0; i < num; i++)
        {
            if ( ! r.isDeleted( i))
            {
                Document d = r.document(i);
                String thisNode = d.get("path");

                // there sometimes is a file "" in the index.
                // this trips up GraphViz, so ignore it.
                if( thisNode.equals("") ) continue;
                
                // output a node for the Document and color code it
                // according to how long it has been since it has last
                // been modified.
                String modified = d.get("modified");
                String colorCode = ageColor( age(modified) );
                cout.println( "    node [style=\"filled\",fillcolor=\"" + colorCode + "\"]" );
                cout.println( "    \"" + thisNode + "\"" );

                // output edges for the links the document contains
                // to other documents.
                String links = d.get("links");
                if( links != null ) {
                    cout.println( formatLinks(thisNode,links) );
                }
                
            }
            cout.println( "" );
        }
        r.close(); 

        cout.println( "}" ); // close the digraph
    }


    /** 
     * Generates DOT code for the edges of a file:
     * @param origin the name of the node where the edges originate from,
     * @param links  a string with the names of the the nodes the edges will
     *               go to, space separated.
     **/
    String formatLinks( String origin, String links ) {
        StringTokenizer t = new StringTokenizer( links );
        String result = "";
        while( t.hasMoreTokens() ) {
            String target = t.nextToken();
            result = result +  "    \"" + origin + "\" -> \"" + target + "\"\n";
        }
        return result;
    }

    /**
     * Generates DOT code for a node that contains legend information.
     **/
    String legendBox() { 
        // the legend box.
        return   "   node [shape=\"box\",style=\"bold\"]\n"
               + "   \"Legend:\\n red = very recently modified\\n"
               +                 "yellow = last modified 3 days ago\\n"
               +                 "blue = not modified last week\"\n"
               + "   node [shape=\"ellipse\"]";
    }

    /**
     * Generates text that describes how to build the generated DOT file
     * into a GIF picture.
     **/
    String usageInfo() {
        return   "The following is a graph in DOT format. For a nice "
               + "result, build it with (for example):\n"
               + "unflatten -c4 -l10 | dot -Tgif\n (pipe the "
               + "DOT source into there and out comes a GIF file.\n\n\n";
    }
    
    /**
     * Generates a HTML/DOT color string given an age in milliseconds.
     **/
    String ageColor( long age ) {

        // scale one week (in milliseconds) to [0,512).
        // (1181250 is (1000*60*60*24*7)/512; perhaps make this configurable)
        double scale = 1.0 / 1181250.0;
        int value = (int)( scale * (double)age );


        // in the following code, there are a lot of hard coded numbers.
        // making them easily configurable would be of no use: the only reason
        // for them to change would be because of a change that is so
        // fundamental that this code would have to be totally replaced
        // anyway.
        // as an indication: 255 is the highest value a color channel can
        // have, 256 is the number of levels a color channel can have and
        // 512 is twice that.
        
        // map [0,512) to nice colors.
        int r, g, b;
        if( value < 256 ) { // red to yellow
            r = 255;
            g = value;
            b = 0;
        } else { // yellow through murky brown to blue
            r = 512-value;
            g = 512-value;
            b = value-256;
        }

        // color clamping
        if( r < 0 )   r = 0;
        if( r > 255 ) r = 255;
        if( g < 0 )   g = 0;
        if( g > 255 ) g = 255;
        if( b < 0 )   b = 0;
        if( b > 255 ) b = 255;

        //generate color code for GraphViz
        return "#"+ doubleDigitHex(r) + doubleDigitHex(g) + doubleDigitHex(b);
    }
    
    /**
     * Turns an integer into a two-character string showing the hex
     * representation of the integer.
     **/
    String doubleDigitHex( int i ) {
        String s = Integer.toHexString(i).toUpperCase();
        if( s.length() == 1 ) return "0" + s;
        return s;
    }

    /**
     * Given a date represented as a string in Lucene date format, returns the
     * number of milliseconds the date is older than the current system time.
     **/
    long age( String luceneDate ) {
        try{
            long modifiedTimestamp = DateField.stringToTime( luceneDate );
            long currentTimestamp = new Date().getTime();
            return currentTimestamp - modifiedTimestamp;
        }catch(NumberFormatException ne) {
            return Long.MAX_VALUE;
        }
    }
}
