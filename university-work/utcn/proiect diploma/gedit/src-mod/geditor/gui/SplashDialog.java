package geditor.gui;

import javax.swing.*;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 6, 2003
 * Time: 9:56:04 PM
 */
public class SplashDialog extends JDialog {
    private JProgressBar progressBar;
    private JLabel msg;

    public SplashDialog(MainFrame parent) {
        super(parent, "Connect", false);
        //inti components
        msg = new JLabel("Autodetect ...");
        msg.setBounds(25, 5, 250, 20);
        //
        progressBar = new JProgressBar(JProgressBar.HORIZONTAL, 0, 100);
        progressBar.setBounds(25, 30, 250, 20);
        progressBar.setStringPainted(true);
        //addd comp
        getContentPane().setLayout(null);
        getContentPane().add(msg);
        getContentPane().add(progressBar);
        //dialog init
        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        setSize(300, 100);
        setResizable(false);
        setLocation(parent.getLocation().x - 150 +parent.getSize().width / 2, parent.getLocation().y -50 + parent.getSize().height / 2);
        setVisible(true);
    }

    public void setMessage(String text) {
        msg.setText(text);

    }

    public void setProgress(int p) {
        progressBar.setValue(p);
    }

    public void terminate() {
        dispose();
    }

    public void delayDisplay(){
    /*  long startTime = System.currentTimeMillis();
      long endTime = System.currentTimeMillis();
      while( (endTime - startTime) < Application.TIMEOUT_SPASH ){
          endTime = System.currentTimeMillis();
      }*/
   }


}
