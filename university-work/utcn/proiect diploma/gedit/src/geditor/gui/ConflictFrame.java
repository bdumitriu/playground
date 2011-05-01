package geditor.gui;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Administrator
 * Date: May 15, 2003
 * Time: 11:33:54 AM
 * To change this template use Options | File Templates.
 */
public class ConflictFrame extends JDialog implements ActionListener{
        private JTextArea textArea;
        private JButton clearButton;
        private JButton closeButton;
        private JScrollBar scrollBar;

        private MainFrame parent;



        public ConflictFrame(MainFrame parent)  {
            super(parent, "Conflict History", false);
            this.parent = parent;
            //create components
            JPanel southPanel = new JPanel();
            clearButton = new JButton("Clear");
            clearButton.addActionListener(this);
            closeButton = new JButton("Close");
            closeButton.addActionListener(this);
            southPanel.add(clearButton);
            southPanel.add(closeButton);
            //create the text area
            textArea = new JTextArea();
            JScrollPane scroll = new JScrollPane(textArea);
            scrollBar = scroll.getVerticalScrollBar();
            scrollBar.setAutoscrolls(true);
            getContentPane().setLayout(new BorderLayout());
            getContentPane().add(scroll, BorderLayout.CENTER);
            getContentPane().add(southPanel, BorderLayout.SOUTH);
            setSize(350, 200);
            setLocation(0,0);
            setVisible(false);
            addWindowListener(new WindowEventHandler());
        }

        class WindowEventHandler extends WindowAdapter{
            public void windowClosing(WindowEvent e){
                        setVisible(false);
                        //parent.getMyMenuBar().setButtonStatus("Conflict",false);
                    }
        }

        public void clearList() {
            textArea.setText("");
            clearButton.setEnabled(false);
        }

        public void addToHistoryList(String msg) {
            textArea.append(msg+"\n");
            scrollBar.setValue(scrollBar.getMaximum());
            if ( ! clearButton.isEnabled() ) clearButton.setEnabled(true);
        }

        public void actionPerformed(ActionEvent e) {
            String str = e.getActionCommand();
            if( str.equals("Clear")){
                clearList();
                return;
            }
            if( str.equals("Close")){
                setVisible(false);
                //parent.getMyMenuBar().setButtonStatus("Conflict",false);
                return;
            }
        }


    }


