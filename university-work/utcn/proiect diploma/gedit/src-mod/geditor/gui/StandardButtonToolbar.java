package geditor.gui;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Feb 24, 2003
 * Time: 2:56:29 PM
 */
public class StandardButtonToolbar extends JToolBar implements ActionListener {
    protected MainFrame parent;
//    protected JButton fileNew;
  //  protected JButton fileOpen;
    protected JButton fileSave;
    //protected JButton filePrint;


    public StandardButtonToolbar(MainFrame parent) {
        super();
        setName("Standard buttons");
        setOrientation(JToolBar.HORIZONTAL);
        this.parent = parent;
	    /*
        fileNew = new JButton(new ImageIcon(Application.class.getResource("resources/images/new.gif")));
        fileNew.setActionCommand("New");
        fileNew.setMnemonic(KeyEvent.VK_N);
        fileNew.setFocusable(false);
        fileNew.addActionListener(this);
        fileNew.setToolTipText("New File");
        add(fileNew);
        fileOpen = new JButton(new ImageIcon(Application.class.getResource("resources/images/open.gif")));
        fileOpen.setFocusable(false);
        fileOpen.setActionCommand("Open");
        fileNew.setMnemonic(KeyEvent.VK_O);
        fileOpen.addActionListener(this);
        fileOpen.setToolTipText("Open File");
        add(fileOpen);
        */
        fileSave = new JButton(new ImageIcon("resources/images/save.gif"));
        fileSave.setFocusable(false);
        fileSave.setActionCommand("Save");
       // fileNew.setMnemonic(KeyEvent.VK_S);
        fileSave.addActionListener(this);
        fileSave.setToolTipText("Save File");
        add(fileSave);
/*
        filePrint = new JButton( new ImageIcon(Application.class.getResource("resources/images/print.gif")));
        filePrint.setActionCommand("Print");
        filePrint.addActionListener(this);
        add(filePrint);
        addSeparator();
*/
        //init
        fileSave.setEnabled(true);

    }

    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
         if (cmd.startsWith("Save"))
            parent.doSave();

    }

    public void setButtonStatus(String buttonName, boolean status) {
            fileSave.setEnabled(status);
    }
}
