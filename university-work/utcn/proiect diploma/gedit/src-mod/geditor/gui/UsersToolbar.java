package geditor.gui;

import geditor.users.User;

import javax.swing.*;
import java.awt.*;
import java.util.Vector;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 5, 2003
 * Time: 10:09:20 AM
 */
public class UsersToolbar extends JToolBar {
    //status
    public final static int ACTIVATED = 10;
    public final static int DEACTIVATED = 11;

    public final static int WIDTH = 150;
    public final static int HEIGHT = 200;

    protected JList list;
    protected JLabel title;
    protected JScrollPane scroll;


    public UsersToolbar() {
        super();
        setName("User list");
        setOrientation(JToolBar.VERTICAL);
        setPreferredSize(new Dimension(WIDTH, HEIGHT));
        //create label
        title = new JLabel("User List");
        title.setForeground(Color.blue);
        //init
        list = new JList();
        scroll = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        setLayout(new BorderLayout());
        add(title, BorderLayout.NORTH);
        add(scroll, BorderLayout.CENTER);
    }

    public void loadSettings() {
	    /*
        if (application.getCurrentFileID() != null) {
            scroll.remove(list);
            remove(scroll);
            revalidate();
            //create list
            int n = ((Vector) application.getFileUserListHashTable().get(application.getCurrentFileID())).size();
            Integer[] intList = new Integer[n];
            for (int i = 0; i < intList.length; i++)
                intList[i] = new Integer(i);
            list = new JList(intList);
            ListRenderer listRenderer = new ListRenderer();
            listRenderer.setPreferredSize(new Dimension(WIDTH, 20));
            list.setCellRenderer(listRenderer);
            scroll = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            add(scroll, BorderLayout.CENTER);
            revalidate();
            repaint();
        }
        */
    }

    public void setStatus(int status) {
        boolean s = (status == ACTIVATED);
        if (s) {

        } else {
            scroll.remove(list);
            remove(scroll);
            list = new JList();
            scroll = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            add(scroll, BorderLayout.CENTER);
            revalidate();
            repaint();
        }
    }

    // renderer
    class ListRenderer extends JLabel
            implements ListCellRenderer {

        public ListRenderer() {
            super();
            setIconTextGap(5);
            setOpaque(true);
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
        }

        public Component getListCellRendererComponent(
                JList list,
                Object value,
                int index,
                boolean isSelected,
                boolean cellHasFocus) {
            int selectedIndex = ((Integer) value).intValue();
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            //Set the icon and text.  If icon was null, say so.
/*
            Vector userList = ((Vector) (application.getFileUserListHashTable().get(application.getCurrentFileID())));
            User user = (User) userList.get(selectedIndex);
            if (user.getUserID().equals(application.getUser().getUserID()))
                setText(user.getUserName() + " ***");
            else
                setText(user.getUserName());
            setIcon(user.getImage());
            setFont(new Font("times", Font.BOLD, 12));
            setForeground(Color.red);
*/
            return this;
        }
    }
}
