package geditor.gui;

import javax.swing.*;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 24, 2003
 * Time: 4:01:10 PM
 */
public class StatusBar extends JComponent {
    protected JLabel label;

    public StatusBar(MainFrame parent) {
        label = new JLabel("",new ImageIcon("resources/userimages/01.gif"),JLabel.LEFT);
        label.setFont(new Font("times",Font.BOLD,12));
        label.setForeground(Color.blue);
        setLayout(new BorderLayout());
        add(label, BorderLayout.CENTER);
        setPreferredSize(new Dimension(parent.getSize().width, 30));
    }

    public void display(String msg) {
        label.setText(msg);
        new StatusBarUpdater();
    }

    public void clear() {
        label.setText("");
        label.setIcon(null);
    }

    class StatusBarUpdater extends Thread {

        public StatusBarUpdater() {
            start();
        }

        public void run() {
  /*          try {
                Thread.sleep(Application.TIMEOUT_STATUSBAR_UPDATE);
                clear();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }*/
        }
    }
}

