package geditor.engine.tree;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 10, 2003
 * Time: 11:48:24 AM
 */
public class InvalidIdException extends Exception {
    protected String msg;

    public InvalidIdException(String msg) {
        this.msg = msg;
    }
    public String toString() {
        return "Ivalid id exception :" + msg;
    }
}
