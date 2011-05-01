package geditor.gui;

import geditor.users.User;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Created by IntelliJ IDEA.
 * User: Administrator
 * Date: Apr 4, 2003
 * Time: 3:05:50 PM
 * To change this template use Options | File Templates.
 */
public class UserFrame extends JFrame implements ActionListener {
    private final static int WIDTH = 350;
    private final static int HEIGHT = 200;


    private JButton ok;
    private JButton cancel;
    private LoginPanel loginPanel;
    // private SettingsPanel settingsPanel;
    private JPanel backPanel;

    private String userName = "";
    private String password = "";

    public UserFrame() {
        super("Welcome to GEdit 3.1 ");
        setSize(WIDTH, HEIGHT);
        setResizable(false);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        //creates panels
        loginPanel = new LoginPanel();
        // settingsPanel = new SettingsPanel();
        backPanel = new JPanel();
        backPanel.setLayout(new BorderLayout());
        backPanel.add(loginPanel);
        //create buttons
        ok = new JButton("Ok");
        ok.addActionListener(this);
        cancel = new JButton("Cancel");
        cancel.addActionListener(this);
        //create south panel
        JPanel southPanel = new JPanel();
        southPanel.add(ok);
        southPanel.add(cancel);

        //init frame
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(backPanel, BorderLayout.CENTER);
        getContentPane().add(southPanel, BorderLayout.SOUTH);
        //Center the window
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension frameSize = getSize();
        if (frameSize.height > screenSize.height) {
            frameSize.height = screenSize.height;
        }
        if (frameSize.width > screenSize.width) {
            frameSize.width = screenSize.width;
        }
        setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);
        setVisible(true);
    }


    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        if (cmd.equals("Ok")) {
            if (userName.equals("") || userName.startsWith(" "))
                JOptionPane.showMessageDialog(this, "<html><font size=3 fgColor=blue>Wrong username<font size=3 fgColor=red></html>", "Input error", JOptionPane.WARNING_MESSAGE);
            else if ( password.equals("guest") ) {
                setVisible(false);
                System.out.println("start");
                //Application.startApplication(userName);
                dispose();
            }else
              JOptionPane.showMessageDialog(this, "<html><font size=3 fgColor=red>Wrong password </font></html>", "Input error", JOptionPane.WARNING_MESSAGE);
            return;
        }
        if (cmd.equals("Cancel")) {
            System.exit(0);
            return;
        }
    }

    class LoginPanel extends JPanel {
        private JTextField username;
        private JPasswordField pass;
        private ImageIcon backImage;

        public LoginPanel() {
            setBackground(Color.white);
            setLayout(null);
            JLabel label1 = new JLabel("Username ");
            label1.setBounds(10, 50, 80, 20);
            JLabel label2 = new JLabel("Password ");
            label2.setBounds(10, 50 + 25, 80, 20);
            username = new JTextField();
            username.setBounds(80, 50, 80, 20);
            username.addFocusListener(new FocusAdapter() {
                public void focusLost(FocusEvent e) {
                    if (!username.getText().equals(""))
                        userName = username.getText();
                }
            });
            pass = new JPasswordField();
            pass.setBounds(80, 50 + 25, 80, 20);
            pass.addFocusListener(new FocusAdapter() {
                public void focusLost(FocusEvent e) {
                    if (!(new String(pass.getPassword()).equals("")))
                        password = new String(pass.getPassword());
                }
                  public void focusGained(FocusEvent e) {
                    pass.setText("");
                }
            });
            add(label1);
            add(label2);
            add(username);
            add(pass);
            username.grabFocus();
            backImage = new ImageIcon("resources/images/collaborative.gif");

        }

        public void paintComponent(Graphics g) {
            g.setColor(Color.white);
            g.fillRect(0, 0, UserFrame.WIDTH, UserFrame.HEIGHT);
            g.drawImage(backImage.getImage(), 150, 0, this);
        }
    }

}

