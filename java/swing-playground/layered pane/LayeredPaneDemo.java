import javax.swing.*;
import javax.swing.border.*;
import javax.accessibility.*;

import java.awt.*;
import java.awt.event.*;

public class LayeredPaneDemo extends JFrame {
    private String[] layerStrings = { "Yellow (0)", "Magenta (1)", 
                                      "Cyan (2)", "Red (3)", 
                                      "Green (4)" };
    private Color[] layerColors = { Color.yellow, Color.magenta, 
                                    Color.cyan, Color.red,
                                    Color.green };

    private JLayeredPane layeredPane;
    private JLabel dukeLabel;

    public LayeredPaneDemo()    {
        super("LayeredPaneDemo");

        //Create and load the duke icon.
        final ImageIcon icon = new ImageIcon("images/dukeWaveRed.gif");

        //Create and set up the layered pane.
        layeredPane = new JLayeredPane();
        layeredPane.setPreferredSize(new Dimension(300, 310));
        layeredPane.setBorder(BorderFactory.createTitledBorder(
                                    "Move the Mouse to Move Duke"));
        layeredPane.addMouseMotionListener(new MouseMotionAdapter() {
            final int XFUDGE = 40;
            final int YFUDGE = 57;
            public void mouseEntered(MouseEvent e) {
                dukeLabel.setLocation(e.getX()-XFUDGE, e.getY()-YFUDGE);
            }
            public void mouseMoved(MouseEvent e) {
                dukeLabel.setLocation(e.getX()-XFUDGE, e.getY()-YFUDGE);
            }
        });

        //This is the origin of the first label added.
        Point origin = new Point(10, 20);

        //This is the offset for computing the origin for the next label.
        int offset = 35;

        //Add several overlapping, colored labels to the layered pane
        //using absolute positioning/sizing.
        for (int i = 0; i < layerStrings.length; i++) {
            JLabel label = createColoredLabel(layerStrings[i],
                                              layerColors[i], origin);
            layeredPane.add(label, new Integer(i));  
            origin.x += offset;
            origin.y += offset;
        }

        //Create and add the Duke label to the layered pane.
        dukeLabel = new JLabel(icon);
        dukeLabel.setBounds(15, 225, 
                            icon.getIconWidth(),
                            icon.getIconHeight());
        layeredPane.add(dukeLabel, new Integer(2), 0);

        //Add control pane and layered pane to frame.
        Container contentPane = getContentPane();
        contentPane.setLayout(new BoxLayout(contentPane,
                                            BoxLayout.Y_AXIS));
        contentPane.add(Box.createRigidArea(new Dimension(0, 10)));
        contentPane.add(createControlPanel());
        contentPane.add(Box.createRigidArea(new Dimension(0, 10)));
        contentPane.add(layeredPane);
    }

    //Create and set up a colored label.
    private JLabel createColoredLabel(String text, 
                                      Color color,
                                      Point origin) {
        JLabel label = new JLabel(text);
        label.setVerticalAlignment(JLabel.TOP);
        label.setHorizontalAlignment(JLabel.CENTER);
        label.setOpaque(true);
        label.setBackground(color);
        label.setForeground(Color.black);
        label.setBorder(BorderFactory.createLineBorder(Color.black));
        label.setBounds(origin.x, origin.y, 140, 140);
        return label;
    }

    //Create the control pane for the top of the frame.
    private JPanel createControlPanel() {
        final JCheckBox onTop = new JCheckBox("Top Position in Layer");
        onTop.setSelected(true);
        onTop.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (onTop.isSelected())
                    layeredPane.moveToFront(dukeLabel);
                else
                    layeredPane.moveToBack(dukeLabel);
            }
        });

        final JComboBox layerList = new JComboBox(layerStrings);
        layerList.setSelectedIndex(2);    //Cyan layer
        layerList.addActionListener(new ActionListener () {
            public void actionPerformed(ActionEvent e) {
                int position = onTop.isSelected() ? 0 : 1;
                layeredPane.setLayer(dukeLabel,
                                     layerList.getSelectedIndex(),
                                     position);
            }
        });
        
        JPanel controls = new JPanel();
        controls.add(layerList);
        controls.add(onTop);
        controls.setBorder(BorderFactory.createTitledBorder(
                                 "Choose Duke's Layer and Position"));
        return controls;
    }

    public static void main(String[] args) {
        JFrame frame = new LayeredPaneDemo();

        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });

        frame.pack();
        frame.setVisible(true);
    }
}
