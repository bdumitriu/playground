package geditor.users;

import javax.swing.*;
import java.io.Serializable;


/**
 * Created by IntelliJ IDEA.
 * networkLAN.User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 28, 2003
 * Time: 2:57:12 PM
 */
public class User implements Serializable , Cloneable{

    private String username;
    private String userID;
    private ImageIcon image;

    public User(String username) {
        this.username = username;
        this.userID = username;
        this.image = new ImageIcon("resources/images/file.gif");
    }

    public String getUserName() {
        return username;
    }

    public String getUserID() {
        return username;
    }

    public ImageIcon getImage() {
        return image;
    }

    public Object clone() throws CloneNotSupportedException{
        User user = (User)super.clone();
        user.userID = userID;
        user.username = username;
        user.image = new ImageIcon(image.getImage());
        return user;
    }

}
