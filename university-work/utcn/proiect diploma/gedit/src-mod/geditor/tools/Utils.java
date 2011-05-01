package geditor.tools;

import geditor.users.User;


import java.util.Vector;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * networkLAN.User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 3, 2003
 * Time: 12:05:18 PM
 */
public class Utils {


     public static String transformFileName(String filename){
        String fname = tranasformBackslashSlash(filename);
        int i = fname.lastIndexOf('/');
         if ( i != -1 )
          fname = fname.substring(i+1);
        return fname;
    }

     public static String tranasformBackslashSlash(String fileID) {
        if ( fileID == null ) return null;
        String s = "";
        for (int i = 0; i < fileID.length(); i++)
            if (fileID.charAt(i) == '\\')
                s += "/";
            else
                s += fileID.charAt(i);
        return s;
    }

    public static Vector sortUser(Vector vector) {
        Object m = null;
        String s1 = null,s2 = null;
        int n = vector.size();
        boolean q = true;
        do {
            q = true;
            for (int i = 0; i < n - 1; i++) {
                s1 = ((User) vector.get(i)).getUserID();
                s2 = ((User) vector.get(i + 1)).getUserID();
                if (s1.compareTo(s2) > 0) {
                    m = vector.remove(i);
                    vector.add(i + 1, m);
                    q = false;
                }
            }
        } while (!q);
        return vector;
    }

    public static Vector sortGroupID(Vector vector) {
        Object m = null;
        String s1 = null,s2 = null;
        int n = vector.size();
        int i1 = 0,i2 = 0;
        boolean q = true;
        do {
            q = true;
            for (int i = 0; i < n - 1; i++) {
                s1 = (String) vector.get(i);
                s2 = (String) vector.get(i + 1);
                s1 = s1.substring(s1.lastIndexOf(".")+1);
                s2 = s2.substring(s2.lastIndexOf(".")+1);
                i1 = Integer.parseInt(s1);
                i2 = Integer.parseInt(s2);
                if (i1 > i2) {
                    m = vector.remove(i);
                    vector.add(i + 1, m);
                    q = false;
                }
            }
        } while (!q);
        return vector;
    }

    public static Vector sort(Vector vector) {
        Object m = null;
        String s1 = null,s2 = null;
        int n = vector.size();
        boolean q = true;
        do {
            q = true;
            for (int i = 0; i < n - 1; i++) {
                s1 = vector.get(i).toString();
                s2 = vector.get(i + 1).toString();
                if ( s1.compareTo(s2) > 0) {
                    m = vector.remove(i);
                    vector.add(i + 1, m);
                    q = false;
                }
            }
        } while (!q);
        return vector;
    }

    /**
     * Gets a vector of integer ,sort it and return the first gap
     * It is used to generate new fileindex
     */
    public static int generateIndex(Vector v) {
        if (v.size() == 0) return 1;
        //we have at least one element
        Vector vect = sort(v);
        //get the first elelemnt
        int x = Integer.parseInt((String) vect.firstElement());
        if (x > 1) return 1;
        //the first elenment is 1
        int x1 = 0,x2 = 0;
        for (int i = 0; i < vect.size() - 1; i++) {
            x1 = Integer.parseInt((String) vect.get(i));
            x2 = Integer.parseInt((String) vect.get(i + 1));
            if ((x1 + 1) != x2) return x1 + 1;
        }
        return Integer.parseInt((String) vect.lastElement()) + 1;
    }

    public static Vector intersect(Vector list1,Vector list2){
        Vector intersecion = new Vector();
        for ( int i = 0 ; i < list1.size(); i ++ ) {
            for ( int j = 0 ; j < list2.size(); j ++ ) {
                if ( ((String)list1.get(i)).equals(list2.get(j)) ) {
                    intersecion.addElement(list1.get(i));
                }
            }
        }
        if ( intersecion.isEmpty() ) return null;
        return intersecion;
    }

    public static Vector reverse(Vector v){
        Vector u = new Vector();
        for (int i = v.size()-1; i>=0; i--)
            u.addElement(u.get(i));
        return u;
    }

}
